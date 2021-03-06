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

-define(RIVER_BORDER, 128.0).
-define(BOTTOM_BORDER, 16.0 * 14).
-define(FRAME, w=>16.0, h=>16.0).
-define(PLAYER_INIT_STATE, #{x=>16.0 * 7, y=>16.0 * 14, ?FRAME,
                             dir=>up, face=>#face{h=2, v=0},
                             jump=>0, dying=>0, score=>0}).

%% face defines which 16*16 pixel area to use for the sprite
-record(face, {h, v}).


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
    case code:priv_dir(frogger) of
        {error, bad_name} ->
            %% When not running as a release; e.g., erl -pa ebin
            PrivDir = "priv";
        PrivDir ->
            %% running in a release
            %% VM knows where the application resides
            ok
    end,
    logger:info("Asset directory: ~s", [PrivDir]),
    {ok, TextureBack} = sdl_texture:create_from_file(Renderer,
                                                     filename:join([PrivDir, "background.png"])),
    {ok, TextureSprites} = sdl_texture:create_from_file(Renderer,
                                                        filename:join([PrivDir, "sprites.png"])),
    loop(#{window=>Window,
           renderer=>Renderer,
           textures=>#{background=>TextureBack, sprites=>TextureSprites},
           highscore=>0,
           player=>?PLAYER_INIT_STATE,
           cars=>[
                  #{?FRAME, x=>0,    y=>16.0 * 9, face=>#face{h=5, v=0}, speed=>-0.5}, %% TruckFront
                  #{?FRAME, x=>16.0, y=>16.0 * 9, face=>#face{h=6, v=0}, speed=>-0.5}, %% TruckBack
                  #{?FRAME, x=>100,  y=>16.0 * 9, face=>#face{h=5, v=0}, speed=>-0.5}, %% TruckFront
                  #{?FRAME, x=>116.0, y=>16.0 * 9, face=>#face{h=6, v=0}, speed=>-0.5}, %% TruckBack
                  #{?FRAME, x=>0,    y=>16.0 * 10, face=>#face{h=8, v=0}, speed=>3.0}, %% GreenWhite
                  #{?FRAME, x=>0,    y=>16.0 * 11, face=>#face{h=7, v=0}, speed=>-1.0}, %% Purple
                  #{?FRAME, x=>75,   y=>16.0 * 11, face=>#face{h=7, v=0}, speed=>-1.0}, %% Purple
                  #{?FRAME, x=>150,  y=>16.0 * 11, face=>#face{h=7, v=0}, speed=>-1.0}, %% Purple
                  #{?FRAME, x=>0,    y=>16.0 * 12, face=>#face{h=4, v=0}, speed=>1.0},  %% Bulldozer
                  #{?FRAME, x=>50,   y=>16.0 * 12, face=>#face{h=4, v=0}, speed=>1.0},  %% Bulldozer
                  #{?FRAME, x=>150,  y=>16.0 * 12, face=>#face{h=4, v=0}, speed=>1.0},  %% Bulldozer
                  #{?FRAME, x=>0,    y=>16.0 * 13, face=>#face{h=3, v=0}, speed=>-1.0}, %% Yellow
                  #{?FRAME, x=>128,  y=>16.0 * 13, face=>#face{h=3, v=0}, speed=>-1.0}  %% Yellow
                 ],
           river=>log(3, 0, 4) ++ log(3, 6, 4) ++ log(3, 11, 4) ++
               (turtle_group(4, 0, 2) ++ turtle_group(4, 4, 2) ++
                    turtle_group(4, 8, 2) ++ turtle_group(4, 12, 2)) ++
               log(5, 0, 6) ++
               log(6, 0, 3) ++ log(6, 4, 3) ++ log(6, 8, 3) ++ log(6, 12, 3) ++
               (turtle_group(7, 0, 3) ++ turtle_group(7, 4, 3) ++
                    turtle_group(7, 8, 3) ++ turtle_group(7, 12, 3))
          }).

loop(State) ->
    State2 = events_loop(State),
    State3 = update_player(State2),
    State4 = update_sprites(State3),
    State5 = check_collision(State4),
    State6 = check_borders(State5),
    render(State6),
    timer:sleep(1000 div 50), %% ~50fps
    loop(State6).


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
        _ -> logger:warning("Unhandled scancode: ~B~n", [Scancode]),
             State
    end;
move_player(State, _Scancode) ->  %% No movement change during jump or dying
    State.


turtle_group(Lane, Pos, 3) ->
    Speed = get_lane_speed(Lane),
    [#{x=>16.0 * Pos,       y=>16.0 * Lane, ?FRAME, face=>#face{h=0, v=5}, speed=>Speed},
     #{x=>16.0 * (Pos + 1), y=>16.0 * Lane, ?FRAME, face=>#face{h=0, v=5}, speed=>Speed},
     #{x=>16.0 * (Pos + 2), y=>16.0 * Lane, ?FRAME, face=>#face{h=0, v=5}, speed=>Speed}];
turtle_group(Lane, Pos, 2) ->
    Speed = get_lane_speed(Lane),
    [#{x=>16.0 * Pos,       y=>16.0 * Lane, ?FRAME, face=>#face{h=0, v=5}, speed=>Speed},
     #{x=>16.0 * (Pos + 1), y=>16.0 * Lane, ?FRAME, face=>#face{h=0, v=5}, speed=>Speed}].

log(Lane, Pos, Length) ->
    Speed = get_lane_speed(Lane),
    [#{x=>16.0 * Pos,       y=>16.0 * Lane, ?FRAME, face=>#face{h=6, v=8}, speed=>Speed}]
        ++ log_middle(Lane, Pos + 1, Speed, Length - 2, [])
        ++ [#{x=>16.0 * (Pos + (Length-1)), y=>16.0 * Lane, ?FRAME,
              face=>#face{h=8, v=8}, speed=>Speed}].

log_middle(_Lane, _Pos, _Speed, Length, Acc) when Length == 0 -> Acc;
log_middle(Lane, Pos, Speed, Length, Acc) ->
    log_middle(Lane, Pos + 1, Speed, Length - 1,
               Acc ++ [#{x=>16.0 * Pos, y=>16.0 * Lane, ?FRAME,
                         face=>#face{h=7, v=8}, speed=>Speed}]
              ).

get_lane_speed(Lane) ->
    case Lane of
        2 -> 0.0;
        3 -> 0.5;
        4 -> -0.5;
        5 -> 2.0;
        6 -> 1.0;
        7 -> -1.0
    end.

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
        4 -> #face{h=0, v=3};
        5 -> #face{h=0, v=3};
        6 -> #face{h=0, v=3}
    end.

update_player(State=#{player:=#{jump:=0, dying:=0, y:=Y}}) when Y < ?RIVER_BORDER ->
    %% In river; moved by log or turtle in lane speed
    Player=maps:get(player, State),
    X = maps:get(x, Player) + get_lane_speed(trunc(Y/16)),
    State#{player := Player#{x => X}};
update_player(State=#{player:=#{jump:=0, dying:=0}}) -> %% Not moving
    State;
update_player(State=#{player:=#{jump:=9, dying:=0}}) -> %% Finished jump
    Player=maps:get(player, State),
    Score=maps:get(score, Player) + 10,
    State#{player := Player#{face=>get_player_face(0), jump=>0, score:=Score}};
update_player(State=#{player:=Player=#{jump:=Jump, dying:=0}}) -> %% Jumping and not dying
    NewFace = get_player_face(Jump),
    NewJump = Jump + 1,
    case maps:get(dir, Player) of
        right -> X = maps:get(x, Player) + 2, Y = maps:get(y, Player);
        left  -> X = maps:get(x, Player) - 2, Y = maps:get(y, Player);
        down  -> X = maps:get(x, Player),     Y = maps:get(y, Player) + 2;
        up    -> X = maps:get(x, Player),     Y = maps:get(y, Player) - 2
    end,
    %%    logger:debug("Player: x:~B,y:~B~n", [trunc(X), trunc(Y)]),
    State#{player := Player#{x => X,
                             y => Y,
                             face => NewFace,
                             jump => NewJump}};
update_player(State=#{player:=Player=#{dying:=6*6}}) -> %% Done dying animation, reset
    Score=max(maps:get(score, Player), maps:get(highscore, State)),
    State#{player := ?PLAYER_INIT_STATE,
           highscore=>Score};
update_player(State=#{player:=Player=#{dying:=Dying}}) -> %% Dying, dont move
    Scale=6,
    NewFace=get_player_splatface(Dying div Scale),
    if Dying == 6 * Scale -> NewDying=6 * Scale;
       true               -> NewDying=Dying + 1
    end,
    State#{player := Player#{face=>NewFace,
                             dying=>NewDying,
                             dir=>up}}.


update_sprites(State) ->
    State#{cars := [move_sprite(C) || C <- maps:get(cars, State)],
           river := [move_sprite(C) || C <- maps:get(river, State)]
          }.

move_sprite(Sprite) ->
    X = maps:get(x, Sprite) + maps:get(speed, Sprite),
    if
        X < -15         -> Sprite#{x=>?WIDTH + 15.0};
        X > ?WIDTH + 15 -> Sprite#{x=>-15.0};
        true            -> Sprite#{x=>X}
    end.


check_collision(State=#{player:=#{dying:=0, y:=Y}}) when Y > ?RIVER_BORDER -> %% At road
    Player=maps:get(player, State),
    Cars=maps:get(cars, State),
    case check_collision(Player, Cars) of
        false -> Dying=0;
        true -> Dying=1
    end,
    State#{player:=Player#{dying=>Dying}};
check_collision(State=#{player:=#{jump:=0, dying:=0, y:=Y}}) when Y < ?RIVER_BORDER -> %% At river
    Player=maps:get(player, State),
    River=maps:get(river, State),
    case check_collision(Player, River) of
        false -> Dying=1;
        true -> Dying=0
    end,
    State#{player:=Player#{dying=>Dying}};
check_collision(State) -> %% No collision check when dying or jumping(river)
    State.

check_collision(Player, ObjectList) ->
    lists:any(fun(C) -> sdl_rect:has_intersection(to_rect(Player), to_rect(C)) end, ObjectList).

check_borders(State=#{player:=Player=#{dying:=0, x:=X}}) when X > ?WIDTH-15.0 ->
    State#{player:=Player#{dying=>1}};
check_borders(State=#{player:=Player=#{dying:=0, x:=X}}) when X < 0 ->
    State#{player:=Player#{dying=>1}};
check_borders(State=#{player:=Player=#{dying:=0, y:=Y}}) when Y > ?BOTTOM_BORDER ->
    State#{player:=Player#{dying=>1}};
check_borders(State) ->
    State.

%% Renders graphics

render(State=#{renderer:=Renderer, textures:=Textures, player:=Player, cars:=Cars, river:=River}) ->
    ok = sdl_renderer:clear(Renderer),
    ok = sdl_renderer:copy(Renderer, maps:get(background, Textures),
                           undefined, scale_rect(#{x=>0, y=>0, w=>?WIDTH, h=>?HEIGHT})),

    ok = render_score(Renderer, maps:get(background, Textures), maps:get(score, Player), 0),
    ok = render_score(Renderer, maps:get(background, Textures), maps:get(highscore, State), 10),
    ok = render_sprites(Renderer, maps:get(sprites, Textures), lists:append(Cars, River)),
    ok = render_player(Renderer, maps:get(sprites, Textures), Player),

    ok = sdl_renderer:present(Renderer).

scale_rect(#{x:=X, y:=Y, w:=W, h:=H}) ->
    #{x=>X * ?SCALE, y=>Y * ?SCALE, w=>W * ?SCALE, h=>H * ?SCALE}.

to_rect(#{x:=X, y:=Y, w:=W, h:=H}) ->
    #{x=>trunc(X), y=>trunc(Y), w=>trunc(W), h=>trunc(H)}.

get_angle(right) -> float(90);
get_angle(left) -> float(270);
get_angle(down) -> float(180);
get_angle(up) -> float(0).

render_score(Renderer, Texture, Score, Position) ->
    ok = render_score_digit(Renderer, Texture, Position + 0, empty),
    ok = render_score_digit(Renderer, Texture, Position + 1, empty),
    ok = render_score_digit(Renderer, Texture, Position + 2, empty),
    ok = render_score_digit(Renderer, Texture, Position + 3, (Score div 10000) rem 10),
    ok = render_score_digit(Renderer, Texture, Position + 4, (Score div 1000) rem 10),
    ok = render_score_digit(Renderer, Texture, Position + 5, (Score div 100) rem 10),
    ok = render_score_digit(Renderer, Texture, Position + 6, (Score div 10) rem 10),
    ok = render_score_digit(Renderer, Texture, Position + 7, Score rem 10),
    ok = render_score_digit(Renderer, Texture, Position + 8, empty),
    ok = render_score_digit(Renderer, Texture, Position + 9, empty),
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
    %%    logger:debug("Player: x=~f, y=~f~n", [maps:get(x, Player), maps:get(y, Player)]),
    sdl_renderer:copy(Renderer, Texture,
                      to_rect(#{?FRAME,
                                x=>Face#face.h * 16,
                                y=>Face#face.v * 16}),
                      scale_rect(to_rect(Player)),
                      get_angle(maps:get(dir, Player)), undefined, [none]).

render_sprites(Renderer, Texture, Sprites) ->
    [ok = sdl_renderer:copy(Renderer, Texture,
                            to_rect(#{?FRAME,
                                      x=>(maps:get(face, S))#face.h * 16,
                                      y=>(maps:get(face, S))#face.v * 16}),
                            scale_rect(to_rect(S))) || S <- Sprites],
    ok.
