def strip_quality_values(s : str) -> None | str :
  t = s.split(";")
  if len(t) == 1 :
    return t[0]
  elif len(t) == 2 :
    quality = t[1]
    if quality[:2] == "q=" :
      try :
        f : float = float(quality[2:])
      except ValueError :
        return None
    return None
  else :
    return None

def strip_content_type(s : str) -> None | str :
  t = s.split(";")
  return t[0]