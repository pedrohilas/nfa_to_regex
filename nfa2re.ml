open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Definição dos tipos *)
type estado = int [@@deriving yojson]
type simbolo = char option [@@deriving yojson]
type transicao = (estado * simbolo) * estado [@@deriving yojson]
type nfa = estado list * char list * transicao list * estado list * estado list [@@deriving yojson]

type re =
  | Vazio
  | Caracter of char
  | Concatenacao of re * re
  | Escolha of re * re
  | Estrela of re

(* Funções para converter re para string *)
let string_of_re =
  let rec aux parenthesize = function
    | Vazio -> ""
    | Caracter c -> String.make 1 c
    | Concatenacao (a, b) ->
        let sa = aux true a in
        let sb = aux true b in
        sa ^ sb
    | Escolha (a, b) ->
        let sa = aux false a in
        let sb = aux false b in
        if parenthesize then "(" ^ sa ^ "|" ^ sb ^ ")" else sa ^ "|" ^ sb
    | Estrela a ->
        let sa = aux false a in
        match a with
        | Caracter _ -> sa ^ "*"  (* Não usa parênteses para símbolos únicos *)
        | Vazio -> ""             (* Ignora estrela em vazio *)
        | _ -> "(" ^ sa ^ ")*"    (* Usa parênteses para expressões compostas *)
  in
  aux false



(* Função para ler múltiplas linhas do terminal *)
let rec read_multiplelines () =
  try
    let line = read_line () in
    line ^ " " ^ read_multiplelines ()
  with End_of_file -> ""

(* Função para inicializar GNFA a partir de NFA *)
let inicializar_gnfa (estados, _simbolos, transicoes, iniciais, finais) =
  let novo_inicial = -1 in
  let novo_final = -2 in
  
  (* Remove transições duplicadas: se dois estados têm transições iguais, mantém apenas uma *)
  let transicoes_unicas =
    List.sort_uniq compare transicoes
  in

  (* Adiciona os novos estados e transições para GNFA *)
  let novas_transicoes =
    List.concat
      [ List.map (fun e -> ((novo_inicial, None), e, Caracter '_')) iniciais;
        List.map (fun e -> ((e, None), novo_final, Caracter '_')) finais;
        List.rev_map (fun ((de, simbolo), para) -> 
          ((de, simbolo), para, Caracter (match simbolo with Some s -> s | None -> '_'))
        ) transicoes_unicas ]
  in
  (novo_inicial :: novo_final :: estados, novas_transicoes, novo_inicial, novo_final)



  let obter_transicao gnfa de para =
    let _, transicoes, _, _ = gnfa in
    
    (* Filtra as transições relevantes entre os estados `de` e `para` *)
    let transicoes_filtradas =
      List.filter (fun ((e1, _), e2, _) -> e1 = de && e2 = para) transicoes
    in
    
    (* Extrai apenas as expressões regulares dessas transições *)
    let re_transicoes = List.map (fun (_, _, re) -> re) transicoes_filtradas in
    
    (* Combina todas as transições, incluindo duplicadas, com `Escolha` *)
    let result = List.fold_right (fun re acc ->
      match acc with
      | Vazio -> re
      | _ -> Escolha (acc, re)  (* Combina com `Escolha`, permitindo duplicadas *)
    ) re_transicoes Vazio
    in
  
    (* Converte o resultado para string e imprime para debug *)
  
    result
  
    let remover_estado gnfa qrip =
      let (estados, transicoes, inicial, final) = gnfa in
      
      let novos_estados = List.filter (fun e -> e <> qrip) estados in
      
      let novas_transicoes =
        List.filter 
          (fun ((de, _), para, _) -> de <> qrip && para <> qrip)
          transicoes
      in
  
      (* Acumulador para novas transições sem duplicadas *)
      let transicoes_a_adicionar = 
        List.fold_left (fun acc ((qin, _), _, _) ->
          List.fold_left (fun acc ((_, _), qout, _) ->
            if qin <> qrip && qout <> qrip then
              let rin = obter_transicao gnfa qin qrip in
              let rloop = obter_transicao gnfa qrip qrip in
              let rout = obter_transicao gnfa qrip qout in
              let new_re =
                match (rin, rloop, rout) with
                | (Vazio, _, _) | (_, _, Vazio) -> Vazio  (* Se `rin` ou `rout` são vazios, toda a expressão fica vazia *)
                | (_, Vazio, _) -> Concatenacao (rin, rout)  (* Se `rloop` é vazio, simplifica para `rin` seguido de `rout` *)
                | _ -> Concatenacao (rin, Concatenacao (Estrela rloop, rout))  (* Caso padrão com `rin`, `rloop*`, e `rout` *)
                        
              in
  
              (* Evitar adicionar duplicadas: verificar se já existe *)
              let already_exists = List.exists (fun ((de, _), para, re) ->
                de = qin && para = qout && re = new_re
              ) acc in
  
              if not already_exists then
                ((qin, None), qout, new_re) :: acc
              else
                acc
            else
              acc
          ) acc (List.filter (fun ((de, _), para, _) -> de = qrip && para <> qrip) transicoes)
        ) [] (List.filter (fun ((de, _), para, _) -> de <> qrip && para = qrip) transicoes)
      in
      
      let resultado_gnfa = (novos_estados, transicoes_a_adicionar @ novas_transicoes, inicial, final) in
      resultado_gnfa
  
  

    let rec gnfa_para_re gnfa =
      let (estados, transicoes, inicial, final) = gnfa in
      
      (* Verificar se restam apenas dois estados *)
      if List.length estados = 2 then (
        (* Obter a última transição direta entre os estados finais *)
        match List.find_opt (fun ((de, _), para, _) -> de = inicial && para = final) transicoes with
        | Some (_, _, re_final) ->
            re_final  (* Retorna o valor final diretamente *)
        | None ->
            Vazio
      ) else
        let qrip = List.fold_right (fun e acc -> if e <> inicial && e <> final && e > acc then e else acc) estados min_int in
        let gnfa_reduzido = remover_estado gnfa qrip in
        gnfa_para_re gnfa_reduzido
    
 

 let verificar_alfabeto simbolos transicoes =
          List.iter (fun ((_, simbolo), _) ->
            match simbolo with
            | Some s when not (List.mem s simbolos) ->
                exit 1
            | _ -> ()
          ) transicoes
        
let nfa_para_re (estados, simbolos, transicoes, iniciais, finais) =
    (* Verificação de alfabeto *)
    verificar_alfabeto simbolos transicoes;
  let gnfa = inicializar_gnfa (estados, simbolos, transicoes, iniciais, finais) in
  let re = gnfa_para_re gnfa in
  re

  let () =
  (* Leitura da entrada JSON e conversão para o tipo NFA *)
  let json_input = read_multiplelines () in
  try
    let yojson_data = Yojson.Safe.from_string json_input in
    let nfa = nfa_of_yojson yojson_data in
    match nfa with
    | nfa_data ->
        let re = nfa_para_re nfa_data in
        let resultado = 
          let re_str = string_of_re re in
          if re_str = "n" then "" else re_str  (* Corrige para "n" caso esteja vazio *)
        in
        print_endline resultado
  with _ ->
()