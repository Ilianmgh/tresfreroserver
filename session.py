import threading

class Session :
  data : dict[str, dict[str, str]]
  generation_lock : threading.Lock # lock for generation function. Allows to use an internal state to generate session ids
  modification_lock : threading.Lock # lock to access/modify the data dictionary
  # If negative, the size is not limited
  max_size : int
  # For session_id generation
  session_id_state : int
  def __init__(self, max_size : int = -1) :
    self.data = {}
    self.generation_lock = threading.Lock()
    self.modification_lock = threading.Lock()
    self.max_size = max_size
    self.session_id_state = 0
  def generate_id(self) -> str :
    """ generates a fresh session id and initialize corresponding session """
    with self.generation_lock :
      res = str(self.session_id_state)
      self.session_id_state += 1
      self.data[res] = {} # Initializing session with empty dictionary
      return res
  def add_entry(self, session_id : str, name : str, value : str) :
    # TODO limit size and maybe timeout...
    with self.modification_lock : # See if it is more pertinent to hold the lock longer when adding a sequence of several bindings to the dictionary
      self.data[session_id][name] = value
  def garbage_collect(self, free_size_required : int) -> None :
    with self.modification_lock :
      print("TODO")
  def is_active_session(self, id : str) -> bool :
    """ [is_active_session(id) = true] iff [id] is a session id of an ongoing session """
    res : bool
    with self.modification_lock :
      res = (id in self.data)
    return res
  def url_encode(self, id : str) -> str :
    """ [url_encode(id)] returns a string reprensenting all the data stored under session id [id],
      as a string in a way it can be decoded by [url_decode] i.e. of the form SESSION&key1=val1&...&keyn=valn"""
    res = "SESSION"
    with self.modification_lock :
      id_data = self.data[id]
      for key in id_data :
        res += f"&{key}={id_data[key]}"
    return res
  def url_decode(self, data : str, session_id : str) -> None :
    """ [url_decode(data, id)] adds bindings described by [data] (of the form SESSION&key1=val1&...&keyn=valn)
      under session id [session_id]. """
    data_split = data.split("&")
    new_bindings : list[tuple[str, str]] = []
    if data_split[0].lower() == "session" :
      for binding in data_split[1:] :
        binding_split = binding.split("=")
        if len(binding_split) != 2 :
          raise ValueError(f"parse_url_dictionary: {binding} in {data} not formatted correctly")
        key, value = binding_split[0], binding_split[1]
        # FIXME There is actually redundancy here, bindings for sessions are of the form SESSION&session_x=1,
        # whereas we could forget the prefix session_ in the variable names, maybe remove it ; it may be outdated
        if key[:8] == "session_" : # len("session_") = 8
          key = key[8:]
        self.add_entry(session_id, key, value)