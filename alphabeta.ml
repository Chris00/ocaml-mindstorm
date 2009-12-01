open Game

let rec alphabeta current_game actor alpha beta =
  if(isWin current_game (List.hd current_game.list_event).raw
       (List.hd current_game.list_event).line ) then
    if actor.fst_player then (1, (List.hd current_game.list_event).raw)
    else (-1, (List.hd current_game.list_event).raw)

  else if (List.length current_game.list_event = 42)
  then (0, (List.hd current_game.list_event).raw)
  else
    (
      let game = copy_game current_game in
      if (actor.fst_player) then
        let rec cut_beta alpha beta value raw_current i =
          if (i < Array.length game.tab) then
            let g = move game i actor in
            let test_value = alphabeta g (not actor.fst_player) alpha beta in
            let (value_temp, raw) = if test_value > value then
              (test_value, i) else (value, raw_current) in
            if value_temp > beta then (value_temp, i)
            else cut_beta (max alpha value_temp) beta value_temp raw (i+1)
          else (value, raw_current) in
        cut_beta alpha beta -1 0 0

      else let rec cut_alpha alpha beta value raw_current i =
        if (i < (Array.length game.tab)) then
          let g = move game i actor in
          let test_value = alphabeta g (not actor.fst_player) alpha beta in
          let (value_temp, raw) = if test_value < value then
            (test_value, i) else (value, raw_current) in
          if value_temp < alpha then (value_temp, i)
          else cut_alpha alpha (min beta value_temp) value_temp raw (i+1)
        else (value, raw_current) in
      cut_alpha alpha beta 1 0 0
    )
;;
