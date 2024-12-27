#include <string.h>
#include <unistd.h>
#include <linux/input.h>
#include <linux/uinput.h>
#include <fcntl.h>

// Perform an IOCTL grab or release on an open keyboard handle
int ioctl_keyboard(int fd, int grab) {
  return ioctl(fd, EVIOCGRAB, grab);
}

// Acquire a file descriptor as a uinput keyboard
int acquire_uinput_keysink(int fd, char *name, int vendor, int product, int version) {

  // Designate fd as a keyboard of all keys
  ioctl(fd, UI_SET_EVBIT, EV_KEY);
  int i;
  for (i=0; i < 256; i++) {
    ioctl(fd, UI_SET_KEYBIT, i);
  }

  // Set the vendor details
  struct uinput_setup usetup;
  memset(&usetup, 0, sizeof(usetup));
  usetup.id.bustype = BUS_USB;
  usetup.id.vendor = vendor;
  usetup.id.product = product;
  usetup.id.version = version;


  strncpy(usetup.name, name, UINPUT_MAX_NAME_SIZE);
  ioctl(fd, UI_DEV_SETUP, &usetup);

  // Create the device
  ioctl(fd, UI_DEV_CREATE);

  return 0;
}

// Release a uinput keyboard
int release_uinput_keysink(int fd) {
  return ioctl(fd, UI_DEV_DESTROY);
}

// Send a keyboard event through a file-descriptor
int send_event(int fd, int type, int code, int val, int s, int us) {
  struct input_event ie;
  ie.type = type;
  ie.code = code;
  ie.value = val;
  ie.time.tv_sec = s;
  ie.time.tv_usec = us;
  return write(fd, &ie, sizeof(ie));
}
