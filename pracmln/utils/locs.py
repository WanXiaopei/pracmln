import os

root = os.path.realpath(os.path.join(os.path.dirname(__file__), "..", ".."))
log_dir = os.path.join(os.getcwd(), "log")

if not os.path.exists(log_dir):
    os.mkdir(log_dir)

trdparty = os.path.join(log_dir, "3rdparty")
examples = os.path.join(log_dir, "examples")
etc = os.path.join(log_dir, "etc")
