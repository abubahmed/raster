open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  let image =
    Image.map image ~f:(fun pixel ->
      let avg_color =
        (Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel) / 3
      in
      avg_color, avg_color, avg_color)
  in
  image
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let%expect_test "grayscale" =
  let ref_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  (* CR leli: call transform here *)
  let my_image =
    Image.load_ppm ~filename:"../images/beach_portrait_gray.ppm"
  in
  (* CR leli: Extract this function out into an image compare function *)
  let result =
    Image.foldi ~init:0 ref_image ~f:(fun ~x ~y acc pixel ->
      let my_pixel = Image.get my_image ~x ~y in
      if not (Pixel.equal my_pixel pixel) then acc + 1 else acc)
  in
  if not (result = 0) then print_s [%message "mismatches = " (result : int)];
  [%expect {||}]
;;
