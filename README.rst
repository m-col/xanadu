xanadu
======

xanadu is a standalone desktop metaphor for desktop environments that do not
bundle one, such as standalone window managers. xanadu acts as an underlay
surface beneath regular windows that displays the contents of a specified
directory as icons. These icons can be activated to open a given file or
folder.

Currently, the displayed folder is ``${XDG_DESKTOP_DIR:-$HOME/Desktop}``.

xanadu is Gtk-based and so the style of icons and possibly other parts will be
determined by your configured Gtk theme.


Usage
-----

xanadu does not launch new processes itself. Instead, it simply prints the full
path of activated items to the standard output stream. This makes it easy to
use better-suited tools to figure out what to do.

For example, this will delegate item handling to the useful ``mimeopen`` script
packaged by many unix distributions ("perl-file-mimeinfo" on Arch Linux), and
folder handling to ``thunar``:

.. code-block:: sh
   xanadu | while read item
   do
       if test -d "$item"
       then
           thunar "$item"
       else
           mimeopen $item
        fi
   done

For inspiration as to what tools to delegate to, I recommend looking at
https://wiki.archlinux.org/title/Default_applications.
