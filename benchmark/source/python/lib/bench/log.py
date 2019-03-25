import sys, argparse


class LoggingArgumentParser(argparse.ArgumentParser):
    """ Subclass of ArgumentParser that outputs errors using the log functions"""
    
    def _print_message(self, message, file = None):
        if message:
            # If no file is given, use stderr
            if file is None:
                file = sys.stderr
                
            if file == sys.stdout:
                # If printing to stdout, use info
                info(message)
            elif file == sys.stderr:
                # If printing to stderr, use error
                error(message)
            else:
                # Otherwise defer to parent
                super(PrettyArgumentParser, self)._print_message(message, file)
                
                
def _supports_color(tty):
    """
    Returns True if the given tty supports color, False otherwise
    """
    return hasattr(tty, 'isatty') and tty.isatty()


def done(msg):
    """Prints a done message to screen.
    """
    
    for line in msg.splitlines():
        if _supports_color(sys.stdout):
            print "\033[35;1m[DONE] %s\033[0m" % line
        else:
            print "[DONE] %s" % line


def info(msg):
    """Prints an informational message to screen.
    """
    
    for line in msg.splitlines():
        if _supports_color(sys.stdout):
            print "\033[34m[INFO] %s\033[0m" % line
        else:
            print "[INFO] %s" % line


def warn(msg):
    """Prints a warning message to screen.
       Use, for example, when an assumption is being made.
    """
    
    for line in msg.splitlines():
        if _supports_color(sys.stderr):
            print >> sys.stderr, "\033[33m[WARNING] %s\033[0m" % line
        else:
            print >> sys.stderr, "[WARNING] %s" % line
        

def error(msg):
    """Prints an error message to screen.
       Use for recoverable errors."""
       
    for line in msg.splitlines():
        if _supports_color(sys.stderr):
            print >> sys.stderr, "\033[31m[ERROR] %s\033[0m" % line
        else:
            print >> sys.stderr, "[ERROR] %s" % line


def fatal(msg):
    """Prints an error message to screen and exits with non-zero exit status.
       Use for non-recoverable errors."""
       
    for line in msg.splitlines():
        if _supports_color(sys.stderr):
            print >> sys.stderr, "\033[31;1m[FATAL ERROR] %s\033[0m" % line
        else:
            print >> sys.stderr, "[FATAL ERROR] %s" % line
    sys.exit(1)
