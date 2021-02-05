data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show,Eq,Enum)

daysInMonth month = case month of
  January -> 31
  February -> 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31


nextDay day month = case month of
  January ->
    if day == daysInMonth January
      then (1, February)
      else (day + 1, January)
  February ->
    if day == daysInMonth February
      then (1, March)
      else (day + 1, February)
  March ->
    if day == daysInMonth March
      then (1, April)
      else (day + 1, March)
  April ->
    if day == daysInMonth April
      then (1, May)
      else (day + 1, April)
  May ->
    if day == daysInMonth May
      then (1, June)
      else (day + 1, May)
  June ->
    if day == daysInMonth June
      then (1, July)
      else (day + 1, June)
  July ->
    if day == daysInMonth July
      then (1, August)
      else (day + 1, July)
  August ->
    if day == daysInMonth August
      then (1, September)
      else (day + 1, August)
  September ->
    if day == daysInMonth September
      then (1, October)
      else (day + 1, September)
  October ->
    if day == daysInMonth October
      then (1, November)
      else (day + 1, October)
  November ->
    if day == daysInMonth November
      then (1, December)
      else (day + 1, November)
  December ->
    if day == daysInMonth December
      then (1, January)
      else (day + 1, December)

  