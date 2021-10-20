%Leonardo NASSABAIN
%Projet Erlang


-module(projet).
-import(lists, [max/1]).
-export([test/1, process/3]).

%Fonction qui crée des listes
make_list(0) ->
    [];
make_list(1) ->
    [0];
make_list(N) ->
    [0 | make_list(N-1)].



%Renvoie une matrice non applatie à partir d'une matrice applatie
unflatten_matrix([], _) ->
    [];
unflatten_matrix(Matrix, Size) ->
    Ligne = lists:sublist(Matrix, Size),
    Reste = lists:nthtail(Size, Matrix),
    [Ligne | unflatten_matrix(Reste,Size)].



%Retrouve l'indice d'un elt dans une matrice applatie
find_index(Index1, Index2, Size) ->
    (Index1-1)*Size + Index2.


%Incremente la matrice lors de l'envoi
incr_mat_send(Matrix, Index1, Index2, Size) ->
   Ind1 = find_index(Index1, Index2, Size),   %(i,j)
   CutL1 = lists:sublist(Matrix, Ind1 - 1),
   CutR1 = lists:nthtail(Ind1, Matrix),
   MatTmp = CutL1 ++ [lists:nth(Ind1, Matrix) + 1] ++ CutR1,
   Ind2 = find_index(Index1, Index1, Size),  %(i,i)
   CutL2 = lists:sublist(MatTmp, Ind2 - 1),
   CutR2 = lists:nthtail(Ind2, MatTmp),
   CutL2 ++ [lists:nth(Ind2, MatTmp) + 1] ++ CutR2.


%Incremente la matrice lors de la reception
incr_mat_rec(Matrix, Index1, Index2, Size) ->
    Ind1 = find_index(Index2, Index1, Size),   %(j,i)
    CutL1 = lists:sublist(Matrix, Ind1 - 1),
    CutR1 = lists:nthtail(Ind1, Matrix),
    MatTmp = CutL1 ++ [lists:nth(Ind1, Matrix) + 1] ++ CutR1,
    Ind2 = find_index(Index1, Index1, Size),  %(i,i)
    CutL2 = lists:sublist(MatTmp, Ind2 - 1),
    CutR2 = lists:nthtail(Ind2, MatTmp),
    CutL2 ++ [lists:nth(Ind2, MatTmp) + 1] ++ CutR2.



%Fonction qui renvoie une matrice qui contient le max, à part pour les 
%indices (i,i) et (j,i)
max_matrice(Mat1, Mat2, IndexI, IndexJ, Size) ->
    max_matrice(Mat1, Mat2, IndexI, IndexJ, Size, []).
max_matrice([], [], _, _, _, Res) ->
    Res;
max_matrice(Mat1, Mat2, IndexI, IndexJ, Size, Res) ->
    Index = length(Mat1),
    IndA = find_index(IndexI, IndexI, Size),
    IndB = find_index(IndexJ, IndexI, Size),
    CasA = Index == IndA,
    CasB = Index == IndB,
    Cond = CasA or CasB,
    if 
        (Cond) -> 
            Elt = lists:nth(Index, Mat1),
            NewMat1 = lists:droplast(Mat1),
            NewMat2 = lists:droplast(Mat2),
            max_matrice(NewMat1, NewMat2, IndexI, IndexJ, Size, [Elt|Res]);
        true ->
            Elt1 = lists:nth(Index, Mat1),
            Elt2 = lists:nth(Index, Mat2),
            Elements = [Elt1, Elt2],
            Max = max(Elements),
            NewMat1 = lists:droplast(Mat1),
            NewMat2 = lists:droplast(Mat2),
            max_matrice(NewMat1, NewMat2, IndexI, IndexJ, Size, [Max|Res])
    end.



%Fonction pour générer les processus
spawnN(N) when N >= 0 -> 
    spawnN(N, N).
spawnN(0,_) ->
    done;
spawnN(K,N) -> 
    Id = N - K + 1,
    Process = list_to_atom("pid" ++ integer_to_list(Id)),
    register(Process, spawn(projet, process, [Id, make_list(N*N), N])),
    io:format("Process ~p created ~n", [Process]),
    spawnN(K-1, N).


%Remplit une liste avec des chiffres de 1 à N
fill_list(0) ->
    [];
fill_list(1) -> 
    [1];
fill_list(N) ->
    [N | fill_list(N-1)].

%Fonction qui verifie la premiere condition de reception
cond1_rec(Mat1, Mat2, IndexI, IndexJ, N) ->
    Ind = find_index(IndexJ, IndexI, N),
    Elt1 = lists:nth(Ind, Mat1),
    Elt2 = lists:nth(Ind, Mat2),
    (Elt1 == (Elt2 + 1)).

%Fonction qui verifie la deuxieme condition de reception
cond2_rec(Mat1, Mat2, IndexI, IndexJ, N) ->
    cond2_rec(Mat1, Mat2, IndexI, IndexJ, N, N, true).
cond2_rec(_, _, _, _, _, _, false) ->
    false;
cond2_rec(_, _, _, _, _, 0, Res) ->
    Res;
cond2_rec(Mat1, Mat2, IndexI, IndexJ, N, IndexK, Res) ->
    if 
    ((IndexK /= IndexI) and (IndexK /= IndexJ)) -> 
        Ind = find_index(IndexK, IndexI, N),
        Elt1 = lists:nth(Ind, Mat1),
        Elt2 = lists:nth(Ind, Mat2),
        if 
        (Elt1 =< Elt2) ->
            %Condition est satisfaite
            cond2_rec(Mat1, Mat2, IndexI, IndexJ, N, IndexK-1, Res);
        true -> 
            %Condition n'est pas satisfaite
            cond2_rec(Mat1, Mat2, IndexI, IndexJ, N, IndexK-1,  false)
        end;
    true -> 
        cond2_rec(Mat1, Mat2, IndexI, IndexJ, N, IndexK-1, Res)
    end.
            



%Fonction pour envoyer des messages
sendMsg([], Matrix, _, _) ->
    Matrix;
sendMsg(ListDest, Matrix, Id, N) ->
    [DestId | T] = ListDest,
    NewMatrix = incr_mat_send(Matrix, Id, DestId, N), 
    Src = list_to_atom("pid" ++ integer_to_list(Id)),
    Dest = list_to_atom("pid" ++ integer_to_list(DestId)),
    io:format("~p sending message to ~p. Matrix clock = ~p~n", [Src, Dest, unflatten_matrix(NewMatrix, N)]),
    Dest ! {message, NewMatrix, Id},
    sendMsg(T, NewMatrix, Id, N).



%Fonction pour recevoir des messages
receiveMsg(_,Id,_, 0) ->
    Dest = list_to_atom("pid" ++ integer_to_list(Id)),
    io:format("Process ~p recevied all messages, stop~n", [Dest]),
    done;
receiveMsg(Matrix, Id, N, Cpt) ->
    receive
        {message, MatrixSrc, SrcId} ->
            Cond1 = cond1_rec(MatrixSrc, Matrix, Id, SrcId, N),
            Cond2 = cond2_rec(MatrixSrc, Matrix, Id, SrcId, N),
            Dest = list_to_atom("pid" ++ integer_to_list(Id)),
            if 
            (Cond1 and Cond2) -> 
                Src = list_to_atom("pid" ++ integer_to_list(SrcId)),
                NewMatrix = incr_mat_rec(Matrix, Id, SrcId, N),
                MatrixMax = max_matrice(NewMatrix, MatrixSrc, Id, SrcId, N),
                io:format("~p received message from ~p. Matrix clock = ~p~n", [Dest, Src, unflatten_matrix(MatrixMax, N)]),
                receiveMsg(MatrixMax, Id, N, Cpt-1);
            true ->
                Dest ! {waiting, MatrixSrc, SrcId}
            end;
        {waiting, MatrixSrc, SrcId} ->
            Cond1 = cond1_rec(MatrixSrc, Matrix, Id, SrcId, N),
            Cond2 = cond2_rec(MatrixSrc, Matrix, Id, SrcId, N),
            Dest = list_to_atom("pid" ++ integer_to_list(Id)),
            if
            (Cond1 and Cond2) ->
                Dest ! {message, MatrixSrc, SrcId};
            true ->
                Dest ! {waiting, MatrixSrc, SrcId}
            end
    end.



%Fonction de gestion d'envoi et de reception pour un processus donné
process(Id, Matrix, N) ->
    ListProc = lists:reverse(fill_list(N)),
    ListDest = lists:delete(Id, ListProc),
    NewMatrix = sendMsg(ListDest, Matrix, Id, N),
    Lg = length(ListDest),
    receiveMsg(NewMatrix, Id, N, Lg).
    

%Fonction pour lancer le programme
test(N) ->
    spawnN(N).
