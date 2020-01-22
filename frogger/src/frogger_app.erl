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

start(_StartType, _StartArgs) ->
    spawn(fun init/0),
    frogger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

init() ->
    ok = sdl:start([video]),
    ok = sdl:stop_on_exit(),
    {ok, Window} = sdl_window:create(<<"Frogger">>, 0, 0, ?WIDTH*?SCALE, ?HEIGHT*?SCALE, []),
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
           player=>#{x=>16*7, y=>16*14, w=>16, h=>16,
                     dir=>up, sprite_h=>2, sprite_v=>0, jump=>0, dying=>0},
           cars=>[
                  #{x=>0,     y=>16*9,  w=>16, h=>16, sprite_h=>5, sprite_v=>0, dir=>left,  speed=>slow},   %% Truck front
                  #{x=>0+16,  y=>16*9,  w=>16, h=>16, sprite_h=>6, sprite_v=>0, dir=>left,  speed=>slow},   %% Truck back
                  #{x=>0+100, y=>16*9,  w=>16, h=>16, sprite_h=>5, sprite_v=>0, dir=>left,  speed=>slow},   %% Truck front
                  #{x=>0+116, y=>16*9,  w=>16, h=>16, sprite_h=>6, sprite_v=>0, dir=>left,  speed=>slow},   %% Truck back
                  #{x=>0,     y=>16*10, w=>16, h=>16, sprite_h=>8, sprite_v=>0, dir=>right, speed=>fast},   %% Green/White
                  #{x=>0,     y=>16*11, w=>16, h=>16, sprite_h=>7, sprite_v=>0, dir=>left,  speed=>normal}, %% Purple
                  #{x=>0+75,  y=>16*11, w=>16, h=>16, sprite_h=>7, sprite_v=>0, dir=>left,  speed=>normal}, %% Purple
                  #{x=>0+150, y=>16*11, w=>16, h=>16, sprite_h=>7, sprite_v=>0, dir=>left,  speed=>normal}, %% Purple
                  #{x=>0,     y=>16*12, w=>16, h=>16, sprite_h=>4, sprite_v=>0, dir=>right, speed=>normal}, %% Bulldozer
                  #{x=>0+50,  y=>16*12, w=>16, h=>16, sprite_h=>4, sprite_v=>0, dir=>right, speed=>normal}, %% Bulldozer
                  #{x=>0+150, y=>16*12, w=>16, h=>16, sprite_h=>4, sprite_v=>0, dir=>right, speed=>normal}, %% Bulldozer
                  #{x=>0,     y=>16*13, w=>16, h=>16, sprite_h=>3, sprite_v=>0, dir=>left,  speed=>normal}, %% Yellow
                  #{x=>0+128, y=>16*13, w=>16, h=>16, sprite_h=>3, sprite_v=>0, dir=>left,  speed=>normal}  %% Yellow
                 ]
          }).

loop(State) ->
    State2 = events_loop(State),
    State3 = update_player(State2),
    State4 = update_cars(State3),
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

move_player(State=#{player:=#{jump:=0}}, Scancode) ->
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
move_player(State, _Scancode) ->  %% No movement change during jump
    State.

get_player_face(Jump) ->
    case Jump of
        1 -> 2;
        2 -> 2;
        3 -> 1;
        4 -> 1;
        5 -> 1;
        6 -> 1;
        7 -> 0;
        8 -> 0
    end.

update_player(State=#{player:=#{jump:=0}}) -> %% Not moving
    State;
update_player(State=#{player:=#{jump:=9}}) -> %% Finished jump
    Player=maps:get(player, State),
    Score=maps:get(score, State) + 10,
    State#{score:=Score, player := Player#{sprite_h=>2, jump=>0}};
update_player(State=#{player:=Player=#{jump:=Jump, dying:=0}}) -> %% Jumping an not dying
    NewFace = get_player_face(Jump),
    NewJump = Jump + 1,
    case maps:get(dir, Player) of
        right -> X = maps:get(x, Player) + 2, Y = maps:get(y, Player);
        left  -> X = maps:get(x, Player) - 2, Y = maps:get(y, Player);
        down  -> X = maps:get(x, Player),     Y = maps:get(y, Player) + 2;
        up    -> X = maps:get(x, Player),     Y = maps:get(y, Player) - 2
    end,
    io:format("Player: x:~B,y:~B,w:~B,h:~B~n", [X,Y,maps:get(w, Player),maps:get(h, Player)]),
    State#{player := Player#{x => X,
                             y => Y,
                             sprite_h => NewFace,
                             jump => NewJump}};
update_player(State) -> %% Dying, dont move
    State.


update_cars(State) ->
    Cars=maps:get(cars, State),
    State#{cars := [update_car(C) || C <- Cars]}.

update_car(Car=#{dir:=left}) ->
    case maps:get(speed, Car) of
        slow -> X=maps:get(x, Car) - 1;
        normal -> X=maps:get(x, Car) - 1;
        fast -> X=maps:get(x, Car) - 3
    end,
    if
        X < -15 -> Car#{x=>?WIDTH+16};
        true -> Car#{x=>X}
    end;
update_car(Car) ->
    case maps:get(speed, Car) of
        slow -> X=maps:get(x, Car) + 1;
        normal -> X=maps:get(x, Car) + 1;
        fast -> X=maps:get(x, Car) + 3
    end,
    if
        X > ?WIDTH+15 -> Car#{x=>-16};
        true -> Car#{x=>X}
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

render(State=#{renderer:=Renderer, textures:=Textures, player:=Player, cars:=Cars}) ->
    ok = sdl_renderer:clear(Renderer),
    ok = sdl_renderer:copy(Renderer, maps:get(background, Textures),
                           undefined, scale_rect(#{x=>0, y=>0, w=>?WIDTH, h=>?HEIGHT})),

    ok = render_score(Renderer, maps:get(background, Textures), maps:get(score, State)),
    ok = render_cars(Renderer, maps:get(sprites, Textures), Cars),
    ok = render_player(Renderer, maps:get(sprites, Textures), Player),

    ok = sdl_renderer:present(Renderer).

scale_rect(#{x:=X, y:=Y, w:=W, h:=H}) ->
    #{x=>X*?SCALE, y=>Y*?SCALE, w=>W*?SCALE, h=>H*?SCALE}.

get_angle(Dir) ->
    case Dir of
        right -> float(90);
        left -> float(270);
        down -> float(180);
        up -> float(0)
    end.

%% To be improved...
render_score(Renderer, Texture, Score) ->
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>10*8, y=>8, w=>8, h=>8}, %% empty box
                           scale_rect(#{x=>0*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>10*8, y=>8, w=>8, h=>8}, %% empty box
                           scale_rect(#{x=>1*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>10*8, y=>8, w=>8, h=>8}, %% empty box
                           scale_rect(#{x=>2*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>((Score div 10000) rem 10)*8, y=>8, w=>8, h=>8},
                           scale_rect(#{x=>3*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>((Score div 1000) rem 10)*8, y=>8, w=>8, h=>8},
                           scale_rect(#{x=>4*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>((Score div 100) rem 10)*8, y=>8, w=>8, h=>8},
                           scale_rect(#{x=>5*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>((Score div 10) rem 10)*8, y=>8, w=>8, h=>8},
                           scale_rect(#{x=>6*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>((Score div 1) rem 10)*8, y=>8, w=>8, h=>8},
                           scale_rect(#{x=>7*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>10*8, y=>8, w=>8, h=>8}, %% empty box
                           scale_rect(#{x=>8*8, y=>8, w=>8, h=>8})),
    ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>10*8, y=>8, w=>8, h=>8}, %% empty box
                           scale_rect(#{x=>9*8, y=>8, w=>8, h=>8})),
    ok.

render_player(Renderer, Texture, Player) ->
    sdl_renderer:copy(Renderer, Texture,
                      #{x=>maps:get(sprite_h, Player)*16, y=>maps:get(sprite_v, Player)*16,
                        w=>16, h=>16},
                      scale_rect(Player),
                      get_angle(maps:get(dir, Player)), undefined, [none]).

render_cars(Renderer, Texture, Cars) ->
    [ok = sdl_renderer:copy(Renderer, Texture,
                           #{x=>maps:get(sprite_h, C)*16, y=>maps:get(sprite_v, C)*16,
                             w=>16, h=>16},
                            scale_rect(C)) || C <- Cars],
    ok.
