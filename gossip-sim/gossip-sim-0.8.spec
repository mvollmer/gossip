%define nam gossip-sim
%define ver 0.8
%define rel 1
%define prefix /usr

Name: gossip-sim
Summary: a synchronous data flow simulator for digital signal processing
Version: %ver
Release: %rel
Copyright: GPL
Group: Applications/Engineering
Source: %{nam}-%{ver}.tar.gz
BuildRoot: /tmp/%{nam}-%{ver}-build
Packager: Jason Cao <jcao@users.sourceforge.net>
URL: http://gossip.sourceforge.net
requires: guile >= 1.4, guile-devel >= 1.4, guile-oops >= 0.9.0, guile-oops >= 0.9.0


%description
 A simulator for synchronous data flow programs as they often appear in digital signal processing. It is implemented as an extension to Guile.

%prep
%setup

%build
CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%prefix
make

%install
make prefix=$RPM_BUILD_ROOT%{prefix} \
ROOT=$RPM_BUILD_ROOT \
install 

%clean
rm -rf $RPM_BUILD_ROOT


%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-, root, root)

%doc AUTHORS COPYING ChangeLog NEWS README  gossip-sim-0.8.spec


%{prefix}/bin/*
%{prefix}/include/gossip/*
%{prefix}/info/*
%{prefix}/lib/*
%{prefix}/libexec/gossip/sim/example/*
%{prefix}/libexec/gossip/sim/test/*
%{prefix}/share/guile/gossip/*

	






