# GNU Guix --- Functional package management for GNU
# Copyright © 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of GNU Guix.
#
# GNU Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

#
# Test 'guix environment'.
#

set -e

guix environment --version

tmpdir="t-guix-environment-$$"
gcroot="t-guix-environment-gc-root-$$"
trap 'rm -r "$tmpdir"; rm -f "$gcroot"' EXIT

mkdir "$tmpdir"

# 'guix environment' launches /bin/sh if 'SHELL' is unset, so export 'SHELL'
# since we know it's valid (build environments lack /bin/sh.)
export SHELL

# Check the environment variables for the bootstrap Guile.
guix environment --bootstrap --ad-hoc guile-bootstrap --pure \
     --search-paths > "$tmpdir/a"
guix environment --bootstrap --ad-hoc guile-bootstrap:out --pure \
     --search-paths > "$tmpdir/b"

# $PATH must appear in the search paths, and nothing else.
grep -E '^export PATH=.*profile/bin' "$tmpdir/a"
test "`wc -l < "$tmpdir/a"`" = 1

# Guile must be on $PATH.
test -x `sed -r 's/^export PATH="(.*)"/\1/' "$tmpdir/a"`/guile

cmp "$tmpdir/a" "$tmpdir/b"

# Check '--preserve'.
GUIX_TEST_ABC=1
GUIX_TEST_DEF=2
GUIX_TEST_XYZ=3
export GUIX_TEST_ABC GUIX_TEST_DEF GUIX_TEST_XYZ
guix environment --bootstrap --ad-hoc guile-bootstrap --pure	\
     --preserve='^GUIX_TEST_A' --preserve='^GUIX_TEST_D'	\
     -- "$SHELL" -c set > "$tmpdir/a"
grep '^PATH=' "$tmpdir/a"
grep '^GUIX_TEST_ABC=' "$tmpdir/a"
grep '^GUIX_TEST_DEF=' "$tmpdir/a"
if grep '^GUIX_TEST_XYZ=' "$tmpdir/a"; then false; else true; fi

# Make sure the exit value is preserved.
if guix environment --bootstrap --ad-hoc guile-bootstrap --pure \
        -- guile -c '(exit 42)'
then
    false
else
    test $? = 42
fi

# Make sure 'GUIX_ENVIRONMENT' points to the profile.
guix environment --bootstrap --ad-hoc guile-bootstrap --pure \
     -- "$SHELL" -c 'test -f "$GUIX_ENVIRONMENT/bin/guile"'

# Make sure 'GUIX_ENVIRONMENT' points to the profile when building from a
# manifest.
echo "(use-modules (guix profiles) (gnu packages bootstrap))

(packages->manifest (list %bootstrap-guile))
" > $tmpdir/manifest.scm
guix environment --bootstrap --manifest=$tmpdir/manifest.scm --pure \
     -- "$SHELL" -c 'test -f "$GUIX_ENVIRONMENT/bin/guile"'

# if not sharing CWD, chdir home
(
  cd "$tmpdir" \
    && guix environment --bootstrap --container --no-cwd --user=foo  \
            --ad-hoc guile-bootstrap --pure \
            -- /bin/sh -c 'test $(pwd) == "/home/foo" -a ! -d '"$tmpdir"
)

# Make sure '-r' works as expected.
rm -f "$gcroot"
expected="`guix environment --bootstrap --ad-hoc guile-bootstrap \
             -- "$SHELL" -c 'echo $GUIX_ENVIRONMENT'`"
guix environment --bootstrap -r "$gcroot" --ad-hoc guile-bootstrap \
     -- guile -c 1
test `readlink "$gcroot"` = "$expected"

# Make sure '-r' is idempotent.
guix environment --bootstrap -r "$gcroot" --ad-hoc guile-bootstrap \
     -- guile -c 1
test `readlink "$gcroot"` = "$expected"
rm "$gcroot"

# Same with an absolute file name.
guix environment --bootstrap -r "$PWD/$gcroot" --ad-hoc guile-bootstrap \
     -- guile -c 1
test `readlink "$gcroot"` = "$expected"

case "`uname -m`" in
    x86_64)
	# On x86_64, we should be able to create a 32-bit environment.
	guix environment --bootstrap --ad-hoc guile-bootstrap --pure	\
	     -- guile -c '(exit (string-prefix? "x86_64" %host-type))'
	guix environment --bootstrap --ad-hoc guile-bootstrap --pure	\
	     -s i686-linux						\
	     -- guile -c '(exit (string-prefix? "i686" %host-type))'
	;;
    *)
	echo "nothing to do" >&2
	;;
esac

# Make sure we can build the environment of 'guix'.  There may be collisions
# in its profile (e.g., for 'gzip'), but we have to accept them.
guix environment guix --bootstrap -n

# Try program transformation options.
mkdir "$tmpdir/emacs-36.8"
drv="`guix environment --ad-hoc emacs -n 2>&1 | grep 'emacs.*\.drv'`"
transformed_drv="`guix environment --ad-hoc emacs --with-source="$tmpdir/emacs-36.8" -n 2>&1 | grep 'emacs.*\.drv'`"
test -n "$drv"
test "$drv" != "$transformed_drv"
case "$transformed_drv" in
    *-emacs-36.8.drv) true;;
    *)                false;;
esac
rmdir "$tmpdir/emacs-36.8"

# Transformation options without '--ad-hoc'.
drv="`guix environment -n emacs-geiser 2>&1 | grep '\.drv$'`"
transformed_drv="`guix environment -n emacs-geiser \
  --with-input=emacs-minimal=vim 2>&1 | grep '\.drv$'`"
test "$drv" != "$transformed_drv"
case "$drv" in
    *-emacs-minimal*.drv*) true;;
    *)                     false;;
esac
case "$transformed_drv" in
    *-emacs-minimal*.drv*) false;;
    *)                     true;;
esac
case "$transformed_drv" in
    *-vim*.drv*) true;;
    *)           false;;
esac


if guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    # Compute the build environment for the initial GNU Make.
    guix environment --bootstrap --no-substitutes --search-paths --pure \
         -e '(@@ (gnu packages commencement) gnu-make-boot0)' > "$tmpdir/a"

    # Make sure bootstrap binaries are in the profile.
    profile=`grep "^export PATH" "$tmpdir/a" | sed -r 's|^.*="(.*)/bin"|\1|'`

    # Make sure the bootstrap binaries are all listed where they belong.
    grep -E "^export PATH=\"$profile/bin\""         "$tmpdir/a"
    grep -E "^export CPATH=\"$profile/include\""    "$tmpdir/a"
    grep -E "^export LIBRARY_PATH=\"$profile/lib\"" "$tmpdir/a"
    for dep in bootstrap-binaries-0 gcc-bootstrap-0 glibc-bootstrap-0
    do
	guix gc --references "$profile" | grep "$dep"
    done

    # 'make-boot0' itself must not be listed.
    if guix gc --references "$profile" | grep make-boot0
    then false; else true; fi

    # Make sure that the shell spawned with '--exec' sees the same environment
    # as returned by '--search-paths'.
    guix environment --bootstrap --no-substitutes --pure \
         -e '(@@ (gnu packages commencement) gnu-make-boot0)' \
         -- /bin/sh -c 'echo $PATH $CPATH $LIBRARY_PATH' > "$tmpdir/b"
    ( . "$tmpdir/a" ; echo $PATH $CPATH $LIBRARY_PATH ) > "$tmpdir/c"
    cmp "$tmpdir/b" "$tmpdir/c"

    rm "$tmpdir"/*

    # Compute the build environment for the initial GNU Findutils.
    guix environment --bootstrap --no-substitutes --search-paths --pure \
         -e '(@@ (gnu packages commencement) findutils-boot0)' > "$tmpdir/a"
    profile=`grep "^export PATH" "$tmpdir/a" | sed -r 's|^.*="(.*)/bin"|\1|'`

    # Make sure the bootstrap binaries are all listed where they belong.
    grep -E "^export PATH=\"$profile/bin\""         "$tmpdir/a"
    grep -E "^export CPATH=\"$profile/include\""    "$tmpdir/a"
    grep -E "^export LIBRARY_PATH=\"$profile/lib\"" "$tmpdir/a"
    for dep in bootstrap-binaries-0 gcc-bootstrap-0 glibc-bootstrap-0 \
				    make-boot0
    do
	guix gc --references "$profile" | grep "$dep"
    done

    # The following test assumes 'make-boot0' has a "debug" output.
    make_boot0_debug="`guix build -e '(@@ (gnu packages commencement) gnu-make-boot0)' | grep -e -debug`"
    test "x$make_boot0_debug" != "x"

    # Make sure the "debug" output is not listed.
    if guix gc --references "$profile" | grep "$make_boot0_debug"
    then false; else true; fi

    # Compute the build environment for the initial GNU Make, but add in the
    # bootstrap Guile as an ad-hoc addition.
    guix environment --bootstrap --no-substitutes --search-paths --pure	\
         -e '(@@ (gnu packages commencement) gnu-make-boot0)'		\
         --ad-hoc guile-bootstrap > "$tmpdir/a"
    profile=`grep "^export PATH" "$tmpdir/a" | sed -r 's|^.*="(.*)/bin"|\1|'`

    # Make sure the bootstrap binaries are all listed where they belong.
    grep -E "^export PATH=\"$profile/bin\""         "$tmpdir/a"
    grep -E "^export CPATH=\"$profile/include\""    "$tmpdir/a"
    grep -E "^export LIBRARY_PATH=\"$profile/lib\"" "$tmpdir/a"
    for dep in bootstrap-binaries-0 gcc-bootstrap-0 glibc-bootstrap-0 \
				    guile-bootstrap
    do
	guix gc --references "$profile" | grep "$dep"
    done

    # Make sure a package list with plain package objects and package+output
    # tuples can be used with -e.
    expr_list_test_code="
(list (@@ (gnu packages commencement) gnu-make-boot0)
      (list (@ (gnu packages bootstrap) %bootstrap-guile) \"out\"))"

    guix environment --bootstrap --ad-hoc --no-substitutes --search-paths \
         --pure -e "$expr_list_test_code" > "$tmpdir/a"
    profile=`grep "^export PATH" "$tmpdir/a" | sed -r 's|^.*="(.*)/bin"|\1|'`

    for dep in make-boot0 guile-bootstrap
    do
	guix gc --references "$profile" | grep "$dep"
    done
fi
