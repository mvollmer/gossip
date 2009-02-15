aclocal &&
libtoolize --copy --automake &&
autoconf && 
automake --add-missing &&
echo Now run configure and make.
