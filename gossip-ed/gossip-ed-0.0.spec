%define nam gossip-ed
%define ver 0.0
%define rel 0
%define prefix /usr

Name: gossip-ed
Summary: the schematic editor of the gossip simulation environment
Version: %ver
Release: %rel
Copyright: GPL
Group: Applications/Engineering
Source: %{nam}-%{ver}.tar.gz
BuildRoot: /tmp/%{nam}-%{ver}-build
Packager: Jason Cao <jcao@users.sourceforge.net>
URL: http://gossip.sourceforge.net
requires: gossip-sim >= 0.8-0, guile-gtk >= 0.19-0

%description
This is going to be the schematic editor of the Gossip simulation
environment.  The plan is to write most of it in Scheme, using a
customized version of the GnomeCanvas widget for the display.  We do
not use GnomeCavas directly because I don't want to have too many
dependencies on external, rapidly evolving packages.  We wont likely
be using the anti-aliased stuff from Raph, as cool as it is.  The
weight for this schematic editor is on fast, informative display that
works well over the wire, and not on the prettiest possible WYSIWIG.

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

%doc AUTHORS COPYING ChangeLog NEWS README  gossip-ed-0.0.spec


%{prefix}/bin/*
%{prefix}/lib/*
%{prefix}/share/guile/gossip/*

	






