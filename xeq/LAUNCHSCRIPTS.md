### Launch Scripts ###

There is no way to obtain command line arguments in Modula-2 in a dialect independent way,
nor is it even possible to do so in a portable manner across different operating systems.

For this reason M2BSK reads its command line arguments from a file called `m2bskargs.tmp`.
A small launch script is therefore required that will echo the command line arguments into
this file, then launch M2BSK and delete the temporary file again after M2BSK has exited.

This directory will contain the launch scripts for different operating systems:

* `mc.sh` for the bash shell used on Unix and Unix-like operating systems
* `mc.bat` for the command interpreter on Windows, MS-DOS and OS/2
* `mc.com` for the DCL command language on OpenVMS

A launch script for AmigaOS shall be added in the future.
