import threading

class Session :
  self.data : dict[str, dict[str, str]]
  self.generation_lock : threading.Lock
  self.modification_lock : threading.Lock
  # If negative, the size is not limited
  self.max_size : int
  # For session_id generation
  self.session_id_state : int
  def __init__(self, max_size : int = -1) :
    self.data = {}
    self.generation_lock = threading.Lock()
    self.modification_lock = threading.Lock()
    self.max_size = max_size
    self.session_id_state = 0
  def generate_id(self) -> str :
    """ generates a fresh session id """
    with generation_lock :
      res = str(self.session_id_state)
      self.session_id_state += 1
      return res
  def add_entry(self, session_id : str, name : str, value : str) :
    # TODO limit size and maybe timeout...
    with modification_lock : # See if it is more pertinent to hold the lock longer when adding a sequence of several bindings to the dictionary
      self.data[session_id][name] = value
  def garbage_collect(self, free_size_required : int) :
    print("TODO")
  def parse_url_dictionary(data : str, session_id : str) :
    """ [parse_url_dictionary(data, id)] adds bindings described by [data] under session id [session_id]. """
    data_split = data.split("&")
    if data_split[0].lower() == "session" : # I actually think it's generic for any method, except for percent-encoding
      for binding in data_split[1:] :
        binding_split = binding.split("=")
        if len(binding_split) != 2 :
          raise ValueError(f"parse_url_dictionary: {binding} in {data} not formatted correctly")
        key, value = binding_split[0], binding_split[1]
        # FIXME There is actually redundancy here, bindings for sessions are of the form SESSION&session_x=1,
        # whereas we could forget the prefix session_ in the variable names, maybe remove it ; it may be outdated
        if key[:8] == "session_" : # len("session_") = 8
          key = key[8:]
        add_entry(session_id, key, value)