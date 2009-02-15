# Generate the building cruft for a freshly checked out guile-gtk
# module.

##
## NOTE: before changing anything here, please read README.gnome-guile
##

aclocal -I . $ACLOCAL_FLAGS &&
libtoolize --copy --automake &&
autoconf && 
automake --add-missing &&
echo Now run configure and make.
