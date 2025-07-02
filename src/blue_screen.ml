open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  let image =
    Image.mapi foreground ~f:(fun ~x ~y pixel ->
      let should_replace =
        Pixel.blue pixel > 3 * (Pixel.red pixel + Pixel.green pixel) / 4
      in
      if should_replace
      then (
        let background_pixel = Image.get background ~x ~y in
        background_pixel)
      else pixel)
  in
  image
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

(* let%expect_test "blue_screen" =
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let my_image =
    Image.load_ppm ~filename:"../images/oz_bluescreen_vfx.ppm"
  in
  Image.compare ~image1:ref_image ~image2:my_image ~count:10;
  [%expect {||}]
;; *)
