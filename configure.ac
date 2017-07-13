AC_INIT([wpoxy], [0.1], [joakim.jalap@fastmail.com])
AM_INIT_AUTOMAKE([foreign])

dnl Check for GNUTLS
AC_SEARCH_LIBS([gnutls_init], [gnutls], [],
  AC_MSG_ERROR([Could not find GNUTLS library]))

dnl Check for libconfig. Annoyingly, a function changed name in 1.5 (without a
dnl deprecated alias). Right now, the version in FreeBSD ports is 1.4 but the
dnl version in the Arch Linux repos is 1.5. So support both.
AC_SEARCH_LIBS([config_lookup_from], [config],
  [
    echo "Configuring for libconfig < 1.5"
    gnatprep -DLIBCONFIG_SETTING_LOOKUP=\"config_lookup_from\" ./src/libconfig.templ ./src/libconfig.adb
  ],[
    AC_SEARCH_LIBS([config_setting_lookup], [config],
    [
      echo "Configuring for libconfig >= 1.5"
      gnatprep -DLIBCONFIG_SETTING_LOOKUP=\"config_setting_lookup\" ./src/libconfig.templ ./src/libconfig.adb
    ], AC_MSG_ERROR([Could not find libconfig]))])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
