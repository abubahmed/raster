open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius = 
  let result =
    Image.mapi image ~f:(fun ~x ~y _ ->
      let x_left = if x - radius < 0 then 0 else x - radius in
      let x_right = if x + radius >= Image.width image then Image.width image - 1 else x + radius in
      let y_top = if y - radius < 0 then 0 else y - radius in
      let y_bottom = if y + radius >= Image.height image then Image.height image - 1 else y + radius in

      let sliced_image = Image.slice image ~x_start:x_left ~x_end:x_right ~y_start:y_top ~y_end:y_bottom in

      let mean_sliced = Image.mean_pixel sliced_image in
      mean_sliced
    )
  in
  result
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;


let%expect_test "blue_screen" =
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let my_image =
    Image.load_ppm ~filename:"../images/beach_portrait_blur.ppm"
  in
  Image.compare ~image1:ref_image ~image2:my_image ~count:10;
  [%expect {||}]
;;

