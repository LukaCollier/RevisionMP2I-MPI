(**** Displaying images ****)

(*#require "graphics"
open Graphics

let () = open_graph ""; resize_window 28 28
let display_image (im : image) : =
  for i = 0 to 27 do
    for j = 0 to 27 do
      let gray = im.(i).(j) in
      set_color (rgb gray gray gray);
      plot i j
    done
  done
*)

(**** Type definitions ****)

type image = int array array
type label = int
type database = image array * label array

(**** Reading files ****)

let read_images (filename : string) : image array =
  let f = open_in filename in

  let magic = input_binary_int f in
  assert (magic = 2051);

  let nb_images = input_binary_int f in
  let nb_rows = input_binary_int f in
  let nb_columns = input_binary_int f in

  let input_image () = Array.init nb_rows (fun i -> Array.init nb_columns (fun j -> input_byte f)) in
  let images = Array.init nb_images (fun i -> input_image ()) in

  close_in f; images

let read_labels (filename : string) : label array =
  let f = open_in filename in

  let magic = input_binary_int f in
  assert (magic = 2049);

  let nb_labels = input_binary_int f in

  let labels = Array.init nb_labels (fun i -> input_byte f) in

  close_in f; labels

(**** Global declaration for images/labels sets ****)

let train_images : image array = read_images "train-images-idx3-ubyte"
let train_labels : label array = read_labels "train-labels-idx1-ubyte"

let test_images : image array = read_images "t10k-images-idx3-ubyte"
let test_labels : label array = read_labels "t10k-labels-idx1-ubyte"

(**** Utility functions ****)

let dimensions (im : image) =
  Array.length im, Array.length im.(0)

let print_matrix (m : int array array) : unit =
  Array.iter (fun r -> Array.iter (fun x -> Printf.printf "%6d" x) r; print_newline ()) m

let confusion_matrix (labels_real : label array) (labels_found : label array) : int array array =
  assert (Array.length labels_real = Array.length labels_found);
  assert false (* TODO 4 *)

(**** Implementation of k-Nearest Neighbours ****)

let distance (im1 : image) (im2 : image) : int =
  assert (dimensions im1 = dimensions im2);
  assert false (* TODO 1 *)

let i_closest (im : image) (images : image array) : int =
  (* renvoie l'indice i de l'image la plus proche de im *)
  assert false (* TODO 2 *)

let classify_1nn (im : image) (images, labels : database) : int =
  assert false (* TODO 3 *)

let k_closest (k : int) (im : image) (images : image array) : int array =
  (* renvoie les k indices des k images les plus proches de l'image im *)
  assert false (* TODO 5 *)

let classify_knn (k : int) (im : image) (images, labels : database) : int =
  assert false (* TODO 6 *)

(**** Testing the algorithms ****)

let evaluate_1nn () = (* Run after TODO 4 *)
  let labels_found = Array.map (fun im -> classify_1nn im (train_images, train_labels)) test_images in
  print_matrix (confusion_matrix test_labels labels_found)

let evaluate_knn k = (* Run after TODO 6 *)
  let labels_found = Array.map (fun im -> classify_knn k im (train_images, train_labels)) test_images in
  print_matrix (confusion_matrix test_labels labels_found)

let best_k (train_images, train_labels : database) (test_images, test_labels : database) : int =
  assert false (* TODO 7 *)
