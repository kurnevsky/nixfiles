{
  runCommand,
  xorg,
  imagemagick,
}:

let
  cursorNames = [
    "default"
    "arrow"
    "top_left_arrow"
    "pointer"
    "hand1"
    "hand2"
    "pointing_hand"
    "watch"
    "wait"
    "progress"
    "xterm"
    "text"
    "ibeam"
    "crosshair"
    "cross"
    "move"
    "grabbing"
    "grab"
    "fleur"
    "all-scroll"
    "n-resize"
    "s-resize"
    "e-resize"
    "w-resize"
    "ne-resize"
    "nw-resize"
    "se-resize"
    "sw-resize"
    "col-resize"
    "row-resize"
    "ew-resize"
    "ns-resize"
    "nesw-resize"
    "nwse-resize"
    "sb_h_double_arrow"
    "sb_v_double_arrow"
    "top_side"
    "bottom_side"
    "left_side"
    "right_side"
    "top_left_corner"
    "top_right_corner"
    "bottom_left_corner"
    "bottom_right_corner"
    "not-allowed"
    "no-drop"
    "dnd-move"
    "dnd-copy"
    "dnd-none"
    "question_arrow"
    "help"
    "context-menu"
    "alias"
    "copy"
    "cell"
    "vertical-text"
    "zoom-in"
    "zoom-out"
    "up_arrow"
  ];
in
runCommand "transparent-cursor-theme"
  {
    nativeBuildInputs = [
      xorg.xcursorgen
      imagemagick
    ];
  }
  ''
    magick -size 1x1 xc:transparent cursor.png
    echo "1 0 0 cursor.png" > cursor.cfg

    dir=$out/share/icons/Transparent
    mkdir -p $dir/cursors
    xcursorgen cursor.cfg $dir/cursors/left_ptr
    for name in ${toString cursorNames}; do
      ln -s left_ptr "$dir/cursors/$name"
    done

    cat > $dir/index.theme <<EOF
    [Icon Theme]
    Name=Transparent
    Comment=Fully transparent cursor theme
    EOF
  ''
