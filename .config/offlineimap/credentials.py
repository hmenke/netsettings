#!/usr/bin/env python

from io import StringIO
from netrc import netrc
from os.path import expanduser, isfile
from subprocess import check_output, PIPE
from sys import argv


credentials = netrc("/dev/null")
file = expanduser("~/.config/offlineimap/netrc")
if isfile(file):
    with open(file) as fp:
        credentials._parse(file, fp, True)
else:
    proc = check_output(["pass", "Accounts/GMail/mutt"])
    with StringIO(proc.decode("utf-8")) as fp:
        credentials._parse("pass", fp, False)


def get_username(host):
    login, _, _ = credentials.hosts[host]
    return login


def get_password(host):
    _, _, password = credentials.hosts[host]
    return password


if __name__ == "__main__":
    host = argv[1]
    login = get_username(host)
    password = get_password(host)
    print("set imap_user = {}".format(login))
    print("set imap_pass = {}".format(password))

    # Decide whether to use the offline or online configuration
    if isfile(expanduser("~/.config/mutt/mbnames")):
        print("set folder = ~/.local/share/offlineimap/GMail")
        print('source "~/.config/mutt/mbnames"')
    else:
        print('set folder = "imaps://{}@imap.gmail.com/"'.format(login))
