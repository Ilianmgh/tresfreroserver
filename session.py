import threading
import sys
from typing import Any
import time

def getrecsizeof(x : Any) -> int :
  """ returns the size of [x] in bytes """
  to_explore = [x]
  seen = set()
  size_acc : int = 0
  while to_explore != [] :
    cur = to_explore.pop()
    if id(cur) not in seen :
      seen.add(id(cur))
      size_acc += sys.getsizeof(cur)
      if isinstance(cur, dict) :
        for key in cur :
          to_explore.append(key)
          to_explore.append(cur[key])
      elif isinstance(cur, list) :
        for v in cur :
          to_explore.append(v)
      elif isinstance(cur, set) :
        for v in cur :
          to_explore.append(v)
      elif isinstance(cur, tuple) :
        for v in cur :
          to_explore.append(v)
  return sys.getsizeof(x)

class DictTrackSize :
  data : dict[Any, Any]
  size : int
  recsize : int
  def __init__(self, d : dict[Any, Any] | None  = None) :
    if d is None :
      self.data = {}
    else :
      self.data = d
    self.size = sys.getsizeof(self.data)
    self.recsize = getrecsizeof(self.data)
  def __contains__(self, key : Any) :
    return key in self.data
  def __iter__(self) :
    return self.data.__iter__()
  def __getitem__(self, key : Any) :
    return self.data[key]
  def __setitem__(self, key : Any, value : Any) :
    # assert self.size == sys.getsizeof(self.data)
    if key in self.data :
      self.recsize -= getrecsizeof(self.data[key])
      self.recsize -= self.size
      self.data[key] = value
      self.size = sys.getsizeof(self.data)
      self.recsize += getrecsizeof(value)
      self.recsize += self.size
    else :
      self.recsize -= self.size
      self.data[key] = value
      self.size = sys.getsizeof(self.data)
      self.recsize += getrecsizeof(key)
      self.recsize += getrecsizeof(value)
      self.recsize += self.size
    # assert self.recsize == getrecsizeof(self.data) not exactly, should modify to not take into account the recorded size
  def pop(self, key : Any) :
    # assert self.size == sys.getsizeof(self.data)
    value = self.data.pop(key)
    self.recsize -= self.size
    self.recsize -= getrecsizeof(key)
    self.recsize -= getrecsizeof(value)
    self.size = sys.getsizeof(self.data)
    self.recsize += self.size
    self.recsize -= getrecsizeof(self.data[key])
    # assert self.recsize == getrecsizeof(self.data) not exactly, should modify to not take into account the recorded size
  def get_recsize(self) :
    return self.recsize

def get_time_s() :
  return time.time_ns() // 1_000_000_000

class Session :
  data : DictTrackSize
  generation_lock : threading.Lock # lock for generation function. Allows to use an internal state to generate session ids
  modification_lock : threading.Lock # lock to access/modify the data dictionary
  # If negative, the size is not limited
  max_size : int
  # For session_id generation
  session_id_state : int
  # the maximum duration of a session, in seconds
  time_before_expiration : int
  def __init__(self, max_size : int = -1, max_duration : int = 30 * 60) :
    self.data = DictTrackSize()
    self.generation_lock = threading.Lock()
    self.modification_lock = threading.Lock()
    self.max_size = max_size
    self.session_id_state = 0
  def generate_id(self) -> str :
    """ generates a fresh session id and initialize corresponding session """
    with self.generation_lock :
      res = str(self.session_id_state)
      self.session_id_state += 1
      self.data[res] = (get_time_s(), DictTrackSize()) # Initializing session with empty dictionary
    return res # TODO SESSION EXPIRATION & EVICTION;;;;;;
  def add_entry(self, session_id : str, name : str, value : str) :
    with self.modification_lock : # See if it is more pertinent to hold the lock longer when adding a sequence of several bindings to the dictionary
      time, session_dict = self.data[session_id]
      session_dict[name] = value
    if 0 <= self.max_size < self.data.get_recsize() :
      self.evict()
  def evict(self) -> None :
    """ Erases sessions until the size of the stored data is less than self.max_size """
    with self.modification_lock :
      ongoing_unexpired_session : list[str] = []
      cur_time = get_time_s()
      for session_id in self.data :
        start_date, x = self.data[session_id]
        remaining_session_time = cur_time - start_time
        if remaining_session_time > time_before_expiration : # If the session expired, it's evicted
          self.data.pop(session_id)
        else : # otherwise, we store its remaining session time in case we need to evict it once expired session are evicted
          ongoing_unexpired_session.append((session_id, remaining_session_time))
      if self.data.get_recsize() > self.max_size : # This way, we don't sort the session ids if there is no need to evict more data
        ongoing_unexpired_session.sort(key = lambda x : x[1])
        for (session_id, time_remaining) in ongoing_unexpired_session :
          if self.data.get_recsize() > self.max_size :
            self.data.pop(session_id)
          else :
            break
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
      size, id_data = self.data[id]
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
          raise ValueError(f"Session.url_decode: {binding} in {data} not formatted correctly")
        key, value = binding_split[0], binding_split[1]
        # FIXME There is actually redundancy here, bindings for sessions are of the form SESSION&session_x=1,
        # whereas we could forget the prefix session_ in the variable names, maybe remove it ; it may be outdated
        if key[:8] == "session_" : # len("session_") = 8
          key = key[8:]
        self.add_entry(session_id, key, value)
  def __contains__(self, session_id : str) -> bool :
    return (session_id in self.data)