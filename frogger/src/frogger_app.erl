%%%-------------------------------------------------------------------
%% @doc frogger
%% @end
%%%-------------------------------------------------------------------

%% identify priv/background.png PNG 224x256
%% identify priv/sprites.png    PNG 160x160  (16x16 each)

-module(frogger_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(SCALE, 2). %% Graphics scaling

-define(KEYCODE_Q, 20).
-define(KEYCODE_RIGHT, 79).
-define(KEYCODE_LEFT, 80).
-define(KEYCODE_DOWN, 81).
-define(KEYCODE_UP, 82).

-define(WIDTH, 224).
-define(HEIGHT, 256).

%% face defines which 16*16 pixel area to use for the sprite
-record(face,{h,v}).


start(_StartType, _StartArgs) ->
    spawn(fun init/0),
    frogger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

init() ->
    ok = sdl:start([video]),
    ok = sdl:stop_on_exit(),
    {ok, Window} = sdl_window:create(<<"Frogger">>, 0, 0, ?WIDTH * ?SCALE, ?HEIGHT * ?SCALE, []),
    {ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated, present_vsync]),
    ok = sdl_renderer:set_draw_color(Renderer, 100, 100, 100, 0),
    logger:info(#{ msg => "get priv" }),
    case code:priv_dir(frogger) of
        {error, bad_name} ->
            % This occurs when not running as a release; e.g., erl -pa ebin
            % Of course, this will not work for all cases, but should account
            % for most
            PrivDir = "priv";
        PrivDir ->
            % In this case, we are running in a release and the VM knows
            % where the application (and thus the priv directory) resides
            % on the file system
            ok
    end,
    {ok, TextureBack} = sdl_texture:create_from_file(Renderer,
                                                     filename:join([PrivDir, "background.png"])),
    {ok, TextureSprites} = sdl_texture:create_from_file(Renderer,
                                                        filename:join([PrivDir, "sprites.png"])),
    loop(#{window=>Window, renderer=>Renderer,
           textures=>#{background=>TextureBack, sprites=>TextureSprites},
           score=>0,
           player=>#{x=>16 * 7, y=>16 * 14, w=>16, h=>16,
                     dir=>up, face=>#face{h=2, v=0}, jump=>0, dying=>0},
           cars=>[
                  #{x=>0,   y=>16 * 9,  w=>16, h=>16, face=>#face{h=5, v=0}, dir=>left,  speed=>slow},   %% Truck front
                  #{x=>16,  y=>16 * 9,  w=>16, h=>16, face=>#face{h=6, v=0}, dir=>left,  speed=>slow},   %% Truck back
                  #{x=>100, y=>16 * 9,  w=>16, h=>16, face=>#face{h=5, v=0}, dir=>left,  speed=>slow},   %% Truck front
                  #{x=>116, y=>16 * 9,  w=>16, h=>16, face=>#face{h=6, v=0}, dir=>left,  speed=>slow},   %% Truck back
                  #{x=>0,   y=>16 * 10, w=>16, h=>16, face=>#face{h=8, v=0}, dir=>right, speed=>fast},   %% Green/White
                  #{x=>0,   y=>16 * 11, w=>16, h=>16, face=>#face{h=7, v=0}, dir=>left,  speed=>normal}, %% Purple
                  #{x=>75,  y=>16 * 11, w=>16, h=>16, face=>#face{h=7, v=0}, dir=>left,  speed=>normal}, %% Purple
                  #{x=>150, y=>16 * 11, w=>16, h=>16, face=>#face{h=7, v=0}, dir=>left,  speed=>normal}, %% Purple
                  #{x=>0,   y=>16 * 12, w=>16, h=>16, face=>#face{h=4, v=0}, dir=>right, speed=>normal}, %% Bulldozer
                  #{x=>50,  y=>16 * 12, w=>16, h=>16, face=>#face{h=4, v=0}, dir=>right, speed=>normal}, %% Bulldozer
                  #{x=>150, y=>16 * 12, w=>16, h=>16, face=>#face{h=4, v=0}, dir=>right, speed=>normal}, %% Bulldozer
                  #{x=>0,   y=>16 * 13, w=>16, h=>16, face=>#face{h=3, v=0}, dir=>left,  speed=>normal}, %% Yellow
                  #{x=>128, y=>16 * 13, w=>16, h=>16, face=>#face{h=3, v=0}, dir=>left,  speed=>normal}  %% Yellow
                 ],
           river=>log(3, 0, normal, 4) ++ log(3, 6, normal, 4) ++ log(3, 11, normal, 4) ++
               turtle_group(2, 4, 0, normal) ++ turtle_group(2, 4, 4, normal) ++ turtle_group(2, 4, 8, normal) ++ turtle_group(2, 4, 12, normal) ++
               log(5, 0, normal, 6) ++
               log(6, 0, normal, 3) ++ log(6, 4, normal, 3) ++ log(6, 8, normal, 3) ++ log(6, 12, normal, 3) ++
               turtle_group(3, 7, 0, normal) ++ turtle_group(3, 7, 4, normal) ++ turtle_group(3, 7, 8, normal) ++ turtle_group(3, 7, 12, normal)
          }).

loop(State) ->
    State2 = events_loop(State),
    State3 = update_player(State2),
    State4 = update_sprites(State3),
    State5 = check_collision(State4),
    render(State5),
    timer:sleep(1000 div 50), %% ~50fps
    loop(State5).


events_loop(State) ->
    case sdl_events:poll() of
        false -> State;
        #{type:=quit} -> terminate();
        #{type:=key_down, scancode:=?KEYCODE_Q} -> terminate();
        #{type:=key_down, scancode:=Scancode} -> move_player(State, Scancode);
        _ -> events_loop(State)
    end.

terminate() ->
    init:stop(),
    exit(normal).

move_player(State=#{player:=#{jump:=0, dying:=0}}, Scancode) ->
    Player=maps:get(player, State),
    case Scancode of
        ?KEYCODE_RIGHT ->
            State#{player := Player#{dir  => right,
                                     jump => 1}};
        ?KEYCODE_LEFT ->
            State#{player := Player#{dir => left,
                                     jump => 1}};
        ?KEYCODE_DOWN ->
            State#{player := Player#{dir => down,
                                     jump => 1}};
        ?KEYCODE_UP ->
            State#{player := Player#{dir => up,
                                     jump => 1}};
        _ -> io:format("Unhandled scancode: ~B~n", [Scancode]),
              State
    end;
move_player(State, _Scancode) ->  %% No movement change during jump or dying
    State.


turtle_group(3, Lane, Pos, Speed) ->
    [#{x=>16 * Pos,       y=>16 * Lane, w=>16, h=>16, face=>#face{h=0, v=5}, dir=>left, speed=>Speed},
     #{x=>16 * (Pos + 1), y=>16 * Lane, w=>16, h=>16, face=>#face{h=0, v=5}, dir=>left, speed=>Speed},
     #{x=>16 * (Pos + 2), y=>16 * Lane, w=>16, h=>16, face=>#face{h=0, v=5}, dir=>left, speed=>Speed}];
turtle_group(2, Lane, Pos, Speed) ->
    [#{x=>16 * Pos,       y=>16 * Lane, w=>16, h=>16, face=>#face{h=0, v=5}, dir=>left, speed=>Speed},
     #{x=>16 * (Pos + 1), y=>16 * Lane, w=>16, h=>16, face=>#face{h=0, v=5}, dir=>left, speed=>Speed}].

log(Lane, Pos, Speed, Length) ->
    [#{x=>16 * Pos,       y=>16 * Lane, w=>16, h=>16, face=>#face{h=6, v=8}, dir=>right, speed=>Speed}] ++
        log_middle(Lane, Pos + 1, Speed, Length - 2, []) ++
        [#{x=>16 * (Pos + (Length-1)), y=>16 * Lane, w=>16, h=>16, face=>#face{h=8, v=8}, dir=>right, speed=>Speed}].

log_middle(_Lane, _Pos, _Speed, Length, Acc) when Length == 0 -> Acc;
log_middle(Lane, Pos, Speed, Length, Acc) ->
    log_middle(Lane, Pos + 1, Speed, Length - 1, Acc++[#{x=>16 * Pos, y=>16 * Lane, w=>16, h=>16, face=>#face{h=7, v=8}, dir=>right, speed=>Speed}]).

get_player_face(Jump) ->
    case Jump of
        0 -> #face{h=2, v=0};
        1 -> #face{h=2, v=0};
        2 -> #face{h=2, v=0};
        3 -> #face{h=1, v=0};
        4 -> #face{h=1, v=0};
        5 -> #face{h=1, v=0};
        6 -> #face{h=1, v=0};
        7 -> #face{h=0, v=0};
        8 -> #face{h=0, v=0}
    end.

get_player_splatface(Id) ->
    case Id of
        0 -> #face{h=0, v=4};
        1 -> #face{h=0, v=4};
        2 -> #face{h=1, v=4};
        3 -> #face{h=2, v=4};
        4 -> #face{h=0, v=3}
    end.

update_player(State=#{player:=#{jump:=0, dying:=0}}) -> %% Not moving
    State;
update_player(State=#{player:=#{jump:=9, dying:=0}}) -> %% Finished jump
    Player=maps:get(player, State),
    Score=maps:get(score, State) + 10,
    State#{score:=Score, player := Player#{face=>get_player_face(0), jump=>0}};
update_player(State=#{player:=Player=#{jump:=Jump, dying:=0}}) -> %% Jumping an not dying
    NewFace = get_player_face(Jump),
    NewJump = Jump + 1,
    case maps:get(dir, Player) of
        right -> X = maps:get(x, Player) + 2, Y = maps:get(y, Player);
        left  -> X = maps:get(x, Player) - 2, Y = maps:get(y, Player);
        down  -> X = maps:get(x, Player),     Y = maps:get(y, Player) + 2;
        up    -> X = maps:get(x, Player),     Y = maps:get(y, Player) - 2
    end,
    io:format("Player: x:~B,y:~B,w:~B,h:~B~n", [X, Y, maps:get(w, Player), maps:get(h, Player)]),
    State#{player := Player#{x => X,
                             y => Y,
                             face => NewFace,
                             jump => NewJump}};
update_player(State=#{player:=Player=#{dying:=Dying}}) -> %% Dying, dont move
    Scale=6,
    NewFace=get_player_splatface(Dying div Scale),
    if Dying == 4 * Scale -> NewDying=4 * Scale;
       true               -> NewDying=Dying + 1
    end,
    State#{player := Player#{face=>NewFace,
                             dying=>NewDying,
                             dir=>up}}.


update_sprites(State) ->
    State#{cars := [move_sprite(C) || C <- maps:get(cars, State)],
           river := [move_sprite(C) || C <- maps:get(river, State)]
          }.

move_sprite(Sprite=#{dir:=left}) ->
    case maps:get(speed, Sprite) of
        slow   -> X=maps:get(x, Sprite) - 1;
        normal -> X=maps:get(x, Sprite) - 1;
        fast   -> X=maps:get(x, Sprite) - 3
    end,
    if
        X < -15 -> Sprite#{x=>?WIDTH + 16};
        true -> Sprite#{x=>X}
    end;
move_sprite(Sprite) ->
    case maps:get(speed, Sprite) of
        slow   -> X=maps:get(x, Sprite) + 1;
        normal -> X=maps:get(x, Sprite) + 1;
        fast   -> X=maps:get(x, Sprite) + 3
    end,
    if
        X > ?WIDTH + 15 -> Sprite#{x=>-16};
        true -> Sprite#{x=>X}
    end.


check_collision(State=#{player:=#{dying:=0}}) ->
    Player=maps:get(player, State),
    Cars=maps:get(cars, State),
    case check_collision(Player, Cars) of
        false -> Dying=0, ok;
        true -> Dying=1, io:format("COLLISION~n")
    end,
    State#{player:=Player#{dying=>Dying}};
check_collision(State) -> %% No collision check when dying
    State.

check_collision(Player, ObjectList) ->
    lists:any(fun(C) -> sdl_rect:has_intersection(Player, C) end, ObjectList).


%% Renders graphics

render(State=#{renderer:=Renderer, textures:=Textures, player:=Player, cars:=Cars, river:=River}) ->
    ok = sdl_renderer:clear(Renderer),
    ok = sdl_renderer:copy(Renderer, maps:get(background, Textures),
                           undefined, scale_rect(#{x=>0, y=>0, w=>?WIDTH, h=>?HEIGHT})),

    ok = render_score(Renderer, maps:get(background, Textures), maps:get(score, State)),
    ok = render_sprites(Renderer, maps:get(sprites, Textures), lists:append(Cars, River)),
    ok = render_player(Renderer, maps:get(sprites, Textures), Player),

    ok = sdl_renderer:present(Renderer).

scale_rect(#{x:=X, y:=Y, w:=W, h:=H}) ->
    #{x=>X * ?SCALE, y=>Y * ?SCALE, w=>W * ?SCALE, h=>H * ?SCALE}.

get_angle(right) -> float(90);
get_angle(left) -> float(270);
get_angle(down) -> float(180);
get_angle(up) -> float(0).

render_score(Renderer, Texture, Score) ->
    ok = render_score_digit(Renderer, Texture, 0, empty),
    ok = render_score_digit(Renderer, Texture, 1, empty),
    ok = render_score_digit(Renderer, Texture, 2, empty),
    ok = render_score_digit(Renderer, Texture, 3, (Score div 10000) rem 10),
    ok = render_score_digit(Renderer, Texture, 4, (Score div 1000) rem 10),
    ok = render_score_digit(Renderer, Texture, 5, (Score div 100) rem 10),
    ok = render_score_digit(Renderer, Texture, 6, (Score div 10) rem 10),
    ok = render_score_digit(Renderer, Texture, 7, Score rem 10),
    ok = render_score_digit(Renderer, Texture, 8, empty),
    ok = render_score_digit(Renderer, Texture, 9, empty),
    ok.

render_score_digit(Renderer, Texture, Position, Digit) when is_integer(Digit) ->
    sdl_renderer:copy(Renderer, Texture,
                      #{x=>Digit * 8, y=>8, w=>8, h=>8},
                      scale_rect(#{x=>Position * 8, y=>8, w=>8, h=>8}));
render_score_digit(Renderer, Texture, Position, _) ->
    sdl_renderer:copy(Renderer, Texture,
                      #{x=>10 * 8, y=>8, w=>8, h=>8}, %% empty box
                      scale_rect(#{x=>Position * 8, y=>8, w=>8, h=>8})).


render_player(Renderer, Texture, Player) ->
    Face = maps:get(face, Player),
    sdl_renderer:copy(Renderer, Texture,
                      #{x=>Face#face.h * 16, y=>Face#face.v * 16,
                        w=>16, h=>16},
                      scale_rect(Player),
                      get_angle(maps:get(dir, Player)), undefined, [none]).

render_sprites(Renderer, Texture, Sprites) ->
    [ok = sdl_renderer:copy(Renderer, Texture,
                            #{x=>(maps:get(face, S))#face.h * 16, y=>(maps:get(face, S))#face.v * 16,
                              w=>16, h=>16},
                            scale_rect(S)) || S <- Sprites],
    ok.
