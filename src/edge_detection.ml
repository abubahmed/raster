open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)

let calculate_gradient (kernel_values : int list) image ~x ~y =
  let sliced_image =
    Image.slice
      image
      ~x_start:(x - 1)
      ~x_end:(x + 1 + 1)
      ~y_start:(y - 1)
      ~y_end:(y + 1 + 1)
  in
  let gradient =
    Image.foldi sliced_image ~init:0 ~f:(fun ~x ~y acc (r, _, _) ->
      match List.nth kernel_values ((y * 3) + x) with
      | Some some_kernel_value -> acc + (some_kernel_value * r)
      | None ->
        print_endline "error";
        acc)
  in
  gradient
;;

let is_border ~x ~y image =
  x = 0 || y = 0 || x = Image.width image - 1 || y = Image.height image - 1
;;

let transform image ~(threshold : float) ~radius =
  let grayscaled_image = Grayscale.transform image in
  let blurred_image = Blur.transform ~radius grayscaled_image in
  let max_value = Image.max_val blurred_image in
  let edge_detected_image =
    Image.mapi blurred_image ~f:(fun ~x ~y (r, _, _) ->
      if is_border ~x ~y blurred_image
      then r, r, r
      else (
        let horizontal_gradient =
          calculate_gradient
            [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ]
            blurred_image
            ~x
            ~y
        in
        let vertical_gradient =
          calculate_gradient
            [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ]
            blurred_image
            ~x
            ~y
        in
        let mag_gradient =
          sqrt
            (Int.to_float
               ((horizontal_gradient * horizontal_gradient)
                + (vertical_gradient * vertical_gradient)))
        in
        if
          Int.of_float mag_gradient
          > Float.to_int (threshold *. Int.to_float max_value)
        then max_value, max_value, max_value
        else 0, 0, 0))
  in
  edge_detected_image
;;

let command =
  Command.basic
    ~summary:"Edge detect an image"
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
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the solarization threshold"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;

(* let%expect_test "edge_detection" =
   let ref_image =
   Image.load_ppm ~filename:"../images/reference-beach_portrait_edge.ppm"
   in
   let my_image =
   Image.load_ppm ~filename:"../images/beach_portrait.ppm"
   |> transform ~threshold:0.4 ~radius:2
   in
   Image.compare_fast ~image1:ref_image ~image2:my_image;
   [%expect {||}]
   ;; *)
