open Core

let check_bounds_min_max ?(max_value = -1) ?(min_value : int = -1) to_check =
  if to_check > max_value && not (max_value = -1)
  then max_value
  else if to_check < min_value && not (min_value = -1)
  then min_value
  else to_check
;;

let check_bounds_coords ~x ~y image =
  x < 0 || x >= Image.width image || y < 0 || y >= Image.height image
;;

let distribute_error_pixel ~x ~y ~fraction image ~diff =
  if check_bounds_coords ~x ~y image
  then (
    let max_value = Image.max_val image in
    let r, _, _ = Image.get image ~x ~y in
    let new_value_raw =
      Float.to_int (Int.to_float r +. (fraction *. Int.to_float diff))
    in
    let new_value = check_bounds_min_max ~max_value new_value_raw in
    Image.set image ~x ~y (new_value, new_value, new_value))
;;

let distribute_error old_value new_value image x y =
  let diff = old_value - new_value in
  let left_x = x - 1 in
  let right_x = x + 1 in
  let bottom_y = y + 1 in
  distribute_error_pixel
    ~x:left_x
    ~y:bottom_y
    ~fraction:(3.0 /. 16.0)
    image
    ~diff;
  distribute_error_pixel ~x:right_x ~y ~fraction:(7.0 /. 16.0) ~diff image;
  distribute_error_pixel
    ~x:right_x
    ~y:bottom_y
    ~fraction:(1.0 /. 16.0)
    ~diff
    image;
  distribute_error_pixel ~x ~y:bottom_y ~fraction:(5.0 /. 16.0) ~diff image
;;

(* This should look familiar by now! *)
let transform image =
  let grayscaled_image = Grayscale.transform image in
  let max_int_value = Image.max_val grayscaled_image in
  Image.mapi grayscaled_image ~f:(fun ~x ~y (r, _, _) ->
    if Float.(Int.to_float r > Int.to_float max_int_value /. 2.0)
    then (
      distribute_error r max_int_value grayscaled_image x y;
      max_int_value, max_int_value, max_int_value)
    else (
      distribute_error r 0 grayscaled_image x y;
      0, 0, 0))
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
(* 
let%expect_test "dither" =
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  let my_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  Image.compare_fast ~image1:ref_image ~image2:my_image;
  [%expect {||}]
;; *)
