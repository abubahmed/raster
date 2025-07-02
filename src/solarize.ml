open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image ~threshold =
  let max_value = Image.max_val image in
  let threshold_value = Int.to_float max_value *. threshold in
  let image =
    Image.map image ~f:(fun (r, g, b) ->
      let find_new_color color =
        if Float.(Int.to_float color >= threshold_value)
        then max_value - color
        else color
      in
      find_new_color r, find_new_color g, find_new_color b)
  in
  image
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"N the solarization threshold"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_solarized.ppm")]
;;

let%expect_test "solarize" =
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-meadow_solarize.ppm"
  in
  let my_image =
    Image.load_ppm ~filename:"../images/meadow.ppm"
    |> transform ~threshold:0.4
  in
  Image.compare_fast ~image1:ref_image ~image2:my_image;
  [%expect {||}]
;;
