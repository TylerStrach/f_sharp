//
// F# image processing functions.
//
// The goal of this project is write a program in F# to perform various operations on
// images stored in PPM format. 
//
// Used online resources to clarify and provide examples to complicated higher order fuctions 
// such as List.mapi and using anonymous lamda functions
//
// Tyler Strach @ UIC
// Spring 2025 CS 341
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  // utility function to ensure no pixel value will exceed 255
  let limit num = 
    if num > 255 then 255
    else num

  //
  // Sepia:
  //
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //
  let rec Sepia (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    
    // takes a 3-tuple of int values, and applies the correct sepia conversion and returns a new 3-tuple of ints
    let applySepia (r, g, b) = 
      let newR = int (0.393*float r + 0.769*float g + 0.189*float b)
      let R = limit newR

      let newG = int (0.349*float r + 0.686*float g + 0.168*float b)
      let G = limit newG

      let newB = int (0.272*float r + 0.534*float g + 0.131*float b)
      let B = limit newB

      (R, G, B)
  
    List.map (List.map applySepia) image // sepia function for each tuple (pixel)

  //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //
  let rec IncreaseIntensity (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (intensity:double)
                    (channel:char) = 

    // helper function to apply ^intensity to correct ^channel 
    let applyIntensity (r, g, b) = 
      // only apply the intensity to the desired r g or b
      match channel with 
      | 'r' -> (limit (int (float r*intensity)), g, b)
      | 'g' -> (r, limit (int (float g*intensity)), b)
      | 'b' -> (r, g, limit (int (float b*intensity)))
      | _ -> (r, g, b) // do nothing if the channel isn't r g or b
    
    List.map (List.map applyIntensity) image // intensity function for each tuple (pixel)

  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 

    List.map List.rev image // flip each row of pixels

  //
  // Rotate180:
  //
  // Rotates the image 180 degrees.
  //
  // Returns: updated image.
  //
  let rec Rotate180 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    
    let flippedRowsImage = FlipHorizontal width height depth image // first flip each the rows individually
    List.rev flippedRowsImage // then flip the order of all the rows

  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) = 

    // returns True if distance between 2 pixels: sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 ) is greater than threshold, False otherwise
    let isDist (x1, y1, z1) (x2, y2, z2) threshold =  
      let r = float (x1 - x2) 
      let g = float (y1 - y2) 
      let b = float (z1 - z2)
      let dist = sqrt ((r * r) + (g * g) + (b * b))
      dist > threshold

    // updates each of the pixels with the correct color based on the distance to the right and bottom pixels
    let updatePixel pixel x y =  
      let rightNeighbor = List.item (x + 1) (List.item y image)  // pixel to the right using passed index (x, y)
      let bottomNeighbor = List.item x (List.item (y + 1) image)  // pixel beneath using passed index (x, y)
      if isDist pixel rightNeighbor threshold || isDist pixel bottomNeighbor threshold then
        (0, 0, 0)  // Black pixel for edge
      else
        (255, 255, 255)  // White pixel for non-edge

    // higher order implentation to use lamda functions with (x, y) to keep track of indicies
    // first iterates over the rows using List.mapi and leaves the bottom row out using List.take
    // then iteates over each pixel, and leaves the last one out, and maps to either (0, 0, 0) or (255, 255, 255)
    List.mapi (fun y row -> List.mapi (fun x pixel -> updatePixel pixel x y) (List.take (width - 1) row) ) (List.take (height - 1) image)  // Take rows until height - 1 (adjust row count)