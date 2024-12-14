---
title: Report for the _proof assistant project_
author: Damien Fromilhague
---


Pour ce TD j'ai travaillé sur l'implémentation d'un assistant de preuve en OCaml. L'objectif principal était de développer un outil interactif capable de vérifier et d'élaborer des preuves, en intégrant des fonctionnalités comme l'inférence et la vérification de types, la gestion des contextes et la représentation des séquents. Ce projet m'a permis de mettre en pratique des concepts fondamentaux du cours, tout en explorant des extensions comme les types dépendants et les nombres naturels. Le rapport qui suit porte sur les parties mises en œuvre, les choix de conception adoptés et les difficultés rencontrées. 

# What was implemented in the project

Les parties obligatoires ont été implémentées, aussi bien pour les types simples que pour les types dépendants. 

## Simple types

J'ai d'abord dû implémenter un assistant de preuves en utilisant les types simples. Une des parties un peu techniques résidait dans l'écriture de la fonction infer_type, définit (comme son nom l'indique) par inférence et qui est cruciale pour toute la partie 1. 
Pour les types simples, j’ai tout réussi à implémenter. J’ai utilisé infer_type et check, qui s’appellent mutuellement pour simplifier le travail. J'ai pu m'appuyer sur le denier TP (3) pour avancer. 

Les définitions des types et des termes avec leurs constructeurs vont définir toute la syntaxe des premières parties 
```ocaml
(** Type variables. *)
type tvar = string

(** Term variables. *)
type var = string

(** Types. *)
(* Question 1.1  *)
(* Simple type *)
type ty = 
  | TVar of tvar
  | Imp of ty * ty
  | And of ty * ty
  | True 
  | Or of ty * ty
  | False
  | Nat

(* Question 1.2 *)

(* lambda-terms *)
type tm = 
  | Var of var
  | App of tm * tm
  | Abs of string * ty * tm
  | Pair of tm * tm
  | Fst of tm
  | Snd of tm 
  | Unit
  | Left of tm * ty
  | Right of ty * tm
  | Case of tm * var * tm * var * tm
  | Absurd of tm * ty
  | Zero 
  | Succ of tm
  | Rec of tm * tm * tm
```

J'ai mis du temps pour comprendre la partie 2 mais en utilisant l’assistant j'ai pu mieux comprendre des tactiques comme intro et elim.
Au début j'avais bcp de mal pour rédiger les différentes preuves. Ensuite c'est devenu plus naturel sauf pour Russel où j'ai vraiment dû bcp plus m'attarder. 

Pour les nombres naturels, je n'ai pas réellement eu de complications.
## Dependent types

J'ai fais les questions obligatoires mais je n'ai pas eu le temps de faire les facultatives, deadline trop serrée, j'ai sous-estimé le temps nécessaire pour faire cette partie. 
...

# Difficulties encountered

Certaines preuves, en particulier avec les types dépendants, ont été difficiles à valider. Je me suis souvent demandé si l’erreur venait de mon raisonnement ou d’un problème dans mon implémentation. Parfois, des démonstrations évidentes pour moi étaient rejetées par l’assistant, ce qui était frustrant.

J’ai eu du mal à écrire les fonctions mutuellement récursives, comme infer_type et check. C’était une notion nouvelle pour moi, et j’ai mis beaucoup de temps à comprendre comment les organiser correctement. Heureusement, le TP3 m’a aidé à débloquer la situation, mais ça a été un moment compliqué.

L’implémentation de la disjonction a aussi été difficile. Je ne comprenais pas bien les notions "à gauche" ou "à droite", ni l’utilité de case. J’ai passé plusieurs heures à redéfinir les constructeurs et les fonctions comme infer_type ou string_of_tm, mais ça ne fonctionnait toujours pas. C’était frustrant, car malgré mes efforts, je n’avançais pas.

J’ai aussi eu du mal avec la gestion des termes non normalisés. Mes preuves échouaient souvent parce que certains termes restaient partiellement réduits. Même si la solution consistait à normaliser les termes globalement, ce n’était pas évident à repérer pendant le codage. Par moment, les résultats semblaient corrects, mais il restait des réductions possibles, ce qui bloquait tout.

Enfin, j'ai eu des soucis d'un point de vue plus formel, je n'avais jamais réellement utilisé github de cette manière. J'avais déjà gérer des projets C++ assez lourds où j'avais appris à faire un CMakeLists, à compiler et exécuter mon code et les fichiers souhaités. Mais là, j'ai mis du temps à comprendre l'interface, à savoir comment je devais compiler mon code, effectuer les tests.

# Implementation choices

Pour les choix d'implémentation je me suis très grandement référé au cours sur les lambda-calculs et sur ce que j'ai pu trouver sur internet lorsque j'ai bloqué pour la notion de dijonction notamment. Du reste, le TD était tout de même très guidé dans la structure donc le choxix d'implémentation m'a semblé être assez limité, j'ai du mal à voir ce que j'aurais pu réellement changé qui aurait transformé la structure de mon assistant. 
J'ai peut-être des parenthèses inutiles et donc un code trop long dans mon string_of_tm. J'ai perdu beaucoup de temps à réfléchir aux règles de hiérachie, à m'embrouiller et j'ai décidé pour être efficace de passer à la suite sans y revenir par manque de temps.

# Possible extensions

Les questions optionnelles déjà, ce serait un bon début. Sinon, j'aurais pu ajouter plus de fonctions intermédiaires : j'ai parfois des fonctions trop longues comme red dans la partie dependant qui rend le code un peu indigeste. 

# Conclusion

Ce TD m'a semblé très long, beaucoup plus que ce que j'avais imaginé il y a un mois, en commençant les 6-7 premières questions sans grande difficulté. J'ai pu cependant comprendre beaucoup de choses que je n'avais pas compris sur la logique computationnelle, et l'intérêt et le rôle d'Agda, que j'avais utilisé dans le TD5 par exemple, mais sans bien comprendre au fond ce que je manipulais. 
