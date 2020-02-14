Location: `/org/mate/terminal/profiles/default/`

# Zenburn

```
background-color = #3F3F3F3F3F3F
foreground-color = #DCDCDCDCCCCC
bold-color = #E3E3CECEABAB
palette = #3F3F3F3F3F3F:#CCCC93939393:#7F7F9F9F7F7F:#E3E3CECEABAB:#DFDFAFAF8F8F:#CCCC93939393:#8C8CD0D0D3D3:#DCDCDCDCCCCC:#3F3F3F3F3F3F:#CCCC93939393:#7F7F9F9F7F7F:#E3E3CECEABAB:#DFDFAFAF8F8F:#CCCC93939393:#8C8CD0D0D3D3:#DCDCDCDCCCCC
```


# gruvbox dark

```
background-color = #282828282828
foreground-color = #EBEBDBDBB2B2
bold-color = #D7D799992121
palette = #282828282828:#CCCC24241D1D:#989897971A1A:#D7D799992121:#454585858888:#B1B162628686:#68689D9D6A6A:#A8A899998484:#929283837474:#FBFB49493434:#B8B8BBBB2626:#FAFABDBD2F2F:#8383A5A59898:#D3D386869B9B:#8E8EC0C07C7C:#EBEBDBDBB2B2
```

# gruvbox light

```
background-color = '#FBFBF1F1C7C7'
foreground-color = '#3C3C38383636'
bold-color = '#D7D799992121'
palette = ["#FBFBF1F1C7C7","#CCCC24241D1D","#989897971A1A","#D7D799992121","#454585858888","#B1B162628686","#68689D9D6A6A","#7C7C6F6F6464","#929283837474","#9D9D00000606","#797974740E0E","#B5B576761414","#070766667878","#8F8F3F3F7171","#42427B7B5858","#3C3C38383636"]
```

# Script

```bash
#!/bin/sh

# based on
# https://github.com/oz123/solarized-mate-terminal/blob/master/solarized-mate.sh

set -e

if ! command -v dconf > /dev/null; then
    echo "dconf not found"
    exit 1
fi

if [[ $1 == "--reset" ]]; then
   dconf reset -f /org/mate/terminal/global/profile-list
   dconf reset -f /org/mate/terminal/profiles
   exit 0
fi

# read profiles as string
PROFILES=`dconf read /org/mate/terminal/global/profile-list`

# add new profiles
if [ -z "${PROFILES}" ]; then
    PROFILES="['default','gruvbox-dark', 'gruvbox-light', 'zenburn']"
elif [[ ${PROFILES} =~ 'gruvbox-dark' || ${PROFILES} =~ 'gruvbox-light' || ${PROFILES} =~ 'zenburn' ]]; then
    echo "Found gruvbox in your themes, refusing to run twice"
    echo "You can reset these setting with"
    echo `basename "$0"` "--reset"
    exit 1
else
    echo ${PROFILES}
    M="'gruvbox-dark', 'gruvbox-light', 'zenburn']"
    PROFILES=${PROFILES/]/, $M}
fi

dconf write /org/mate/terminal/global/profile-list "${PROFILES}"
dconf read /org/mate/terminal/global/profile-list

PROFILE="gruvbox-dark"
BGCOLOR="#282828282828"
FGCOLOR="#EBEBDBDBB2B2"
BOLDCOLOR="#D7D799992121"
COLORS="#282828282828:#CCCC24241D1D:#989897971A1A:#D7D799992121:#454585858888:#B1B162628686:#68689D9D6A6A:#A8A899998484:#929283837474:#FBFB49493434:#B8B8BBBB2626:#FAFABDBD2F2F:#8383A5A59898:#D3D386869B9B:#8E8EC0C07C7C:#EBEBDBDBB2B2"

# Do not use theme color
dconf write /org/mate/terminal/profiles/${PROFILE}/use-theme-colors false
# set palette
dconf write /org/mate/terminal/profiles/${PROFILE}/palette \"${COLORS}\"
# set highlighted color to be different from foreground-color
dconf write /org/mate/terminal/profiles/${PROFILE}/bold-color-same-as-fg false
dconf write /org/mate/terminal/profiles/${PROFILE}/background-color \"${BGCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/foreground-color \"${FGCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/bold-color \"${BOLDCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/visible-name \"${PROFILE}\"
# other settings
dconf write /org/mate/terminal/profiles/${PROFILE}/default-show-menubar false
dconf write /org/mate/terminal/profiles/${PROFILE}/scrollback-unlimited true
dconf write /org/mate/terminal/profiles/${PROFILE}/scrollbar-position \"hidden\"

PROFILE="gruvbox-light"
BGCOLOR="#FBFBF1F1C7C7"
FGCOLOR="#3C3C38383636"
BOLDCOLOR="#D7D799992121"
COLORS="#FBFBF1F1C7C7:#CCCC24241D1D:#989897971A1A:#D7D799992121:#454585858888:#B1B162628686:#68689D9D6A6A:#7C7C6F6F6464:#929283837474:#9D9D00000606:#797974740E0E:#B5B576761414:#070766667878:#8F8F3F3F7171:#42427B7B5858:#3C3C38383636"

# Do not use theme color
dconf write /org/mate/terminal/profiles/${PROFILE}/use-theme-colors false
# set palette
dconf write /org/mate/terminal/profiles/${PROFILE}/palette \"${COLORS}\"
# set highlighted color to be different from foreground-color
dconf write /org/mate/terminal/profiles/${PROFILE}/bold-color-same-as-fg false
dconf write /org/mate/terminal/profiles/${PROFILE}/background-color \"${BGCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/foreground-color \"${FGCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/bold-color \"${BOLDCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/visible-name \"${PROFILE}\"
# other settings
dconf write /org/mate/terminal/profiles/${PROFILE}/default-show-menubar false
dconf write /org/mate/terminal/profiles/${PROFILE}/scrollback-unlimited true
dconf write /org/mate/terminal/profiles/${PROFILE}/scrollbar-position \"hidden\"

PROFILE="zenburn"
BGCOLOR="#3F3F3F3F3F3F"
FGCOLOR="#DCDCDCDCCCCC"
BOLDCOLOR="#E3E3CECEABAB"
COLORS="#3F3F3F3F3F3F:#CCCC93939393:#7F7F9F9F7F7F:#E3E3CECEABAB:#DFDFAFAF8F8F:#CCCC93939393:#8C8CD0D0D3D3:#DCDCDCDCCCCC:#3F3F3F3F3F3F:#CCCC93939393:#7F7F9F9F7F7F:#E3E3CECEABAB:#DFDFAFAF8F8F:#CCCC93939393:#8C8CD0D0D3D3:#DCDCDCDCCCCC"

# Do not use theme color
dconf write /org/mate/terminal/profiles/${PROFILE}/use-theme-colors false
# set palette
dconf write /org/mate/terminal/profiles/${PROFILE}/palette \"${COLORS}\"
# set highlighted color to be different from foreground-color
dconf write /org/mate/terminal/profiles/${PROFILE}/bold-color-same-as-fg false
dconf write /org/mate/terminal/profiles/${PROFILE}/background-color \"${BGCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/foreground-color \"${FGCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/bold-color \"${BOLDCOLOR}\"
dconf write /org/mate/terminal/profiles/${PROFILE}/visible-name \"${PROFILE}\"
# other settings
dconf write /org/mate/terminal/profiles/${PROFILE}/default-show-menubar false
dconf write /org/mate/terminal/profiles/${PROFILE}/scrollback-unlimited true
dconf write /org/mate/terminal/profiles/${PROFILE}/scrollbar-position \"hidden\"
```