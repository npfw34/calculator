%%% @author Oleg Ivanov <oleg.ivanov@motorolasolutions.com>
%%%
%%% @copyright 2013 Motorola Solutions Ltd.
%%%
%%% @doc  Telnet calculator handler implementation
%%% This module is a part of the final test exercise for the training "Erlang basics + OTP"
%%%
%%%
%%% @end
%%%

-module(calc_handler).
-export([start_link/4, init/4]).

-compile([{parse_transform, lager_transform}]).

-define(SUCCESS,2).
-define(FAILURE,1).
-define(CHECK_INT,0).
-define(CHECK_FLOAT,2).
-define(CHECK_VAR,3).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

%% @doc Initialization routine
%%      @equiv init(Ref, Socket, Transport, Opts)
%% @end
init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
  lager:info("New client arrived at Socket:~p via Transport:~p",[Socket,Transport]),
	loop(Socket, Transport).

%% @doc Main loop
%%      @equiv loop(Socket, Transport)
%% @end
loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
      Result = input_handler(Data),
			Transport:send(Socket, <<Result/binary,"\r","\n">>),
			loop(Socket, Transport);
		_ ->
    loop (Socket, Transport)
	end.

%% @doc Input handler wrapper parsing received binary
%%      @equiv input_handler(Binary)
%% @end
input_handler(Binary) ->
  %%remove spaces and ending symbols if any
  BinaryNoSpaces = binary:replace(Binary,[<<32>>,<<"\r">>,<<"\n">>],<<>>,[global]),
  {Var,BinaryToProcess} = get_variable(BinaryNoSpaces),
  try (
        begin
          CheckedBin = check_input_binary(BinaryToProcess),
          lager:debug("Checked binary: ~p ",[CheckedBin]),
          Ret = calculate(calculate_inside_brackets(CheckedBin)),
          %%save received variable  and the result
          case Var of
            none ->
              Ret;
            _->
              lager:debug("Saving variable:~p and Ret:~p",[Var,Ret]),
              ets:insert(vartable, {Var,Ret}),
              lager:debug("Done"),
              Ret
          end
        end
      ) of
     RetVal->RetVal
  catch
    _:_-> lager:error("Error expression:~p",[BinaryNoSpaces]) ,
    <<"error expression">>
  end.
  %%<< <<(process_byte(Byte))>> || <<Byte>>  <= Input, (<<Byte>> =/= <<"\n">>) and (<<Byte>> =/= <<"\r">>) >>.


%% @doc  The routine calculates an expression inside brackets
%%      @equiv calculate_inside_brackets(Binary)
%% @end
calculate_inside_brackets(Binary)->
    case(binary:match(Binary,[<<"(">>],[])) of
        nomatch ->
          Binary;
        StartPos ->
          case(binary:match(Binary,[<<")">>],[])) of
            nomatch ->
              {error,no_end_bracket};
            EndPos->
              lager:debug("Input Binary:~p.Start:~p,End:~p",[Binary,StartPos,EndPos]),
               %%calculate the expression length
               Pos = {element(1,StartPos),((element(1,EndPos)-element(1,StartPos))+1)},
               lager:debug("New Position: ~p",[Pos]),
               %%calculate the result inside brackets
               Result = calculate(binary:part(Binary,Pos)),
               lager:debug("Result: ~p",[Result]),
               %%replace brackets by the calculated result
               NewBinary = binary:replace(Binary,binary:part(Binary,Pos),Result,[global]),
               lager:debug("New Binary: ~p",[NewBinary]),
               %%call again
              calculate_inside_brackets(NewBinary)
         end
    end .

%% @doc  Common expression calculation routine
%%      @equiv calculate(Expr)
%% @end
calculate(Expr)->
  %%remove brackets if any
  NewExpr = binary:replace(Expr,[<<"(">>,<<")">>],<<>>,[global]),
  lager:debug("Input Expression: ~p ",[NewExpr]),
  %%check if input binary is variable

  case length(ArgsMul = binary:split(NewExpr,[<<"*">>],[global])) of
    ?SUCCESS ->
       [A,B] = ArgsMul,
       %%lager:info("Arguments of the * operation: A:~p,B:~p",[binary_to_integer(<<A>>),binary_to_integer(<<B>>)]),
       Res = binary_to_integer(A)*binary_to_integer(B),
       lager:debug("Result: ~p",[Res]),
       integer_to_binary(Res);
    ?FAILURE ->
       case length(ArgsAdd = binary:split(NewExpr,[<<"+">>],[global])) of
         ?SUCCESS ->
           [A,B] = ArgsAdd,
            Res = binary_to_integer(A)+binary_to_integer(B),
            lager:debug("Result: ~p",[Res]),
            integer_to_binary(Res);
         ?FAILURE ->
             case length(ArgsSub = binary:split(NewExpr,[<<"-">>],[global])) of
               ?SUCCESS ->
                  [A,B] = ArgsSub,
                  Res = binary_to_integer(A)-binary_to_integer(B),
                  lager:debug("Result: ~p",[Res]),
                  integer_to_binary(Res);
               ?FAILURE ->
                 case length(ArgsDiv = binary:split(NewExpr,[<<"/">>],[global])) of
                   ?SUCCESS ->
                       [A,B] = ArgsDiv,
                       Res = binary_to_integer(A)/binary_to_integer(B),
                       lager:debug("Result: ~p",[Res]),
                       case is_integer(Res)  of
                          true  ->  integer_to_binary(Res);
                          false -> float_to_binary(Res,[{decimals, 2}])
                       end;
                   ?FAILURE ->
                      lager:error("Operation unknown"),
                      <<>>
                   end
             end
       end
   end.
%% @doc  Comprehension of the input binary
%%      @equiv check_input_binary(Binary)-
%% @end
check_input_binary(Binary)->
<< <<(
       begin
          case Res = replace_variable(<<Byte>>,0) of
            <<R>> ->
                lager:debug("check_input_binary success"),
                R;
            Ret ->
               lager:error("Input binary error",[Ret]),
               throw(error)
            end
       end
     )>> || <<Byte>>  <= Binary >>.

%% @doc  The function checks input binary and replaces variables by values from ets table
%%      @equiv replace_variable(Byte,Cnt)
%% @end
replace_variable(Byte,Op) ->
  lager:debug("Binary data:~p",[Byte]),
  case Byte of
    <<"(">> ->
      Byte;
    <<")">> ->
      Byte;
    <<"/">> ->
      Byte;
    <<"*">> ->
      Byte;
    <<"+">> ->
      Byte;
    <<"-">> ->
      Byte;
    _->
        case Op of
          ?CHECK_INT ->
              try binary_to_integer(Byte) of
                _Res ->
                  lager:debug("binary_to_integer success"),
                  Byte
              catch
                _:_->
                  lager:debug("binary_to_integer error"),
                  replace_variable(Byte,?CHECK_FLOAT)
              end;
          ?CHECK_FLOAT->
              try binary_to_float(Byte) of
                  _Res ->
                    lager:debug("binary_to_float success"),
                    {error,float_unsupported}
              catch
                _:_->
                  lager:debug("binary_to_float error"),
                  replace_variable(Byte,?CHECK_VAR)
              end;
          ?CHECK_VAR->
              %%ok,it is a variable probably - try to find it in ets table
              case ets:lookup(vartable, Byte) of
                  []->
                    lager:debug("ets:lookup error"),
                    {error,unknown_variable};
                  [{Byte,R}] ->
                    lager:debug("ets variable value:~p",[R]),
                    R ;
                  Unkn->
                      lager:debug("ets returned:~p",[Unkn])
              end;
            _->
               {error,unknown_binary}
        end
  end.
%% @doc  The function splits an input expression like p = <<"1">> + <<"2">> into two parts p and <<"1">> + <<"2"
%%      @equiv get_variable(Binary)
%% @end
get_variable(Binary)->
  case length(Expr = binary:split(Binary,[<<"=">>],[global])) of
    ?SUCCESS ->
      [Var,RestBin]  = Expr,
      {Var,RestBin};
    ?FAILURE ->
      {none,Binary}
  end.

