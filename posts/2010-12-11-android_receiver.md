---
title: Android Receiver
tags: android, linux
---

[Android notifier][] is a great little app I just recently found on the 
marketplace. What it does is use your wifi network or a bluetooth 
connection to send out a broadcast when certain events happen on your 
phone.

The idea is to have a companion application running on your computer to 
listen for the event and pass along the message via some notification 
system: Growl on Windows/Mac and (I think) gnome-dbus on Linux.

This means your phone can be charging in the other room while you're at 
your computer and you'll get a nice notification on your desktop when 
someone's calling you or you get a text.

This is great and all but totally not worth the Gnome or bluetooth 
library dependencies to get going on Linux. After a brief look at the 
project's wiki however I knew I could do something simpler.

I was able to put together two scripts I had already in place to achieve 
a dead-simple android-receiver on my desktop. The first was a script 
call `ncom` which used `netcat` to send commands across a network and 
execute them on another machine. The second, `bashnotify`, was something 
I was playing around with to get pop-up notifications on track changes 
in mpd.

## Netcat

From the project wiki, I found out that the application will send a 
broadcast packet on port 10600 in a specific format. After some playing 
around with test messages I was able to put together the following which 
successfully `echo`d back the message text in a terminal.

```bash 
while read -d $'\0'; do
  echo $REPLY
done < <(netcat -z -u -l -p 10600 localhost)
```

The incoming message doesn't end with a newline but rather a null 
character. That's why using `read -d $'\0'` and `netcat`s `-z` option is 
required. I also found out that I wasn't getting anything from TCP even 
though the android app should be broadcasting with both protocols. Using 
UDP via the `-u` option seems stable so far.

## Dzen

I took the dzen code present in `bashnotify` and tweaked it a little bit 
so that the notification temporarily covers my entire status bar and 
shows the message text:

```bash 
handle_dzen() {
  local message="$*"

  # dzen settings
  local pipe='/tmp/android-receiver.fifo'
  local delay=4
  local x_offset=0
  local y_offset=0
  local height=17
  local font='Verdana-8'
  local foreground='#ffffba'
  local background='#303030'

  if [[ ! -e "$pipe" ]]; then
    mkfifo "$pipe"
    (dzen2 -ta l -h $height -x $x_offset -y $y_offset \
        -fn "$font" -bg $background -fg $foreground < "$pipe"; rm -f "$pipe") &
  fi

  # todo: make this prettier
  (echo "$message"; sleep $delay) >> "$pipe"
}
```

And there you go.

The end product is no longer moving my charger away from its normal 
spot because I'm expecting a call. Instead, I'll see this:

![Android Receiver Screenshot](https://images.pbrisbin.com/android_receiver/android_receiver.png)\ 

The source for this script can be found in my [github][].

<div class="well">
In my continued attempts to learn some C, I decided to combine the 
netcat and message parsing functions of the above into a small C app.

The end result is a nice little program that you can find 
[here][android-receiver]. It handles binding to the port, parsing and 
formatting the message, then handing it off as the first argument to a 
handler script which is in charge of actually displaying the 
notification to the user.

To match this functionality, I've culled the original script down to 
only the `handle_dzen()` function and renamed it to `dzen-handler` such 
that it can be used by any application that wants to toss up a brief 
notification. This script is also available in that android-receiver 
repo.
</div>

[Android notifier]: http://code.google.com/p/android-notifier/ "android notifier"
[github]:           https://github.com/pbrisbin/scripts/blob/e94b24edd89f26b19c88e6d81d8ea7332358f937/android-receiver "my github"
[android-receiver]: https://github.com/pbrisbin/android-receiver "android receiver"
