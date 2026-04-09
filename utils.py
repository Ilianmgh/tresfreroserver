from threading import Lock

def strip_quality_values(s : str) -> None | str :
  """ [strip_quality_values(s)] strips quality values a value of the field 'accept' of an HTTP request.
    Example: strip_quality_values("text/html;q=0.5") returns "text/html" """
  t = s.split(";")
  if len(t) == 1 :
    return t[0]
  elif len(t) == 2 :
    quality = t[1]
    if quality[:2] == "q=" :
      try :
        f : float = float(quality[2:])
        return t[0]
      except ValueError :
        return None
    return None
  else :
    return None

def extract_content_type(s : str) -> str :
  """ [extract_content_type(s)] is similar to [strip_quality_values(s)] except it just returns whatever was before the first ';' regardless of the kind of information after.
    Example: extract_content_type("text/html;charset=utf-8") returns "text/html" whereas strip_quality_values("text/html;charset=utf-8") returns None. """
  t = s.split(";")
  return t[0]

def parse_cookies(data : list[str]) -> dict[str, str] :
  """ parses cookies from a list of the form ["key=value", ...] """
  res : dict[str, str] = {}
  for binding in data :
    binding_split = binding.split("=")
    if len(binding_split) == 2 :
      res[binding_split[0]] = binding_split[1]
  return res

def url_encoding(namespace : str, d : dict[str, str]) -> str :
  res = namespace
  for key in d :
    res += f"&{key}={d[key]}"
  return res

""" [get_first_line_and_rest(s)] returns [(first_line, rest)] where [first_line]
  is the first line of string [s] and [rest] is the remainder of the string.
  The [\n] used to delimit does not appear in any of the two returned strings. """
def get_first_line_and_rest(s : str) -> tuple[str, str] :
  n = len(s)
  i = s.find('\n')
  if i == -1 :
    return (s, "")
  else :
    return (s[:i], s[i+1:])
