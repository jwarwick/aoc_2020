-- AoC 2020, Day 22
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Day is
  package TIO renames Ada.Text_IO;

  package Deck_Vectors is new Ada.Containers.Vectors
    (Index_Type => Natural,
    Element_Type => Natural);
  use Deck_Vectors;

  type State is record
    player1 : Deck_Vectors.Vector;
    player2 : Deck_Vectors.Vector;
  end record;

  function state_hash(item : in State) return Hash_Type is
    h : Unbounded_String := Null_Unbounded_String;
  begin
    for c of item.player1 loop
      h := h & c'IMAGE;
    end loop;
    h := h & '-';
    for c of item.player2 loop
      h := h & c'IMAGE;
    end loop;
    return Ada.Strings.Hash(to_string(h));
  end state_hash;

  package State_Sets is new Ada.Containers.Hashed_Sets
    (Element_Type        => State,
    Hash                => state_hash,
    Equivalent_Elements => "=");
  
  use State_Sets;

  pragma Warnings (Off, "procedure ""put_line"" is not referenced");
  procedure put_line(d : in Deck_Vectors.Vector) is
  pragma Warnings (On, "procedure ""put_line"" is not referenced");
  begin
    for card of d loop
      TIO.put(card'IMAGE & ", ");
    end loop;
    TIO.new_line;
  end put_line;

  procedure load_deck(file : in out TIO.File_Type; p : in out Deck_Vectors.Vector) is
  begin
    p.clear;
    while not TIO.end_of_file(file) loop
      declare
        line : constant String := TIO.get_line(file);
      begin
        if line'length = 0 then
          exit;
        end if;
        if line(1) /= 'P' then
          p.append(Natural'Value(line));
        end if;
      end;
    end loop;
  end load_deck;

  procedure load_decks(filename : in String; p1, p2 : in out Deck_Vectors.Vector) is
    file : TIO.File_Type;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    load_deck(file, p1);
    load_deck(file, p2);
    TIO.close(file);
  end load_decks;

  function score(deck : in Deck_Vectors.Vector) return Natural is
    cnt : Natural := Natural(deck.length);
    sum : Natural := 0;
  begin
    for c of deck loop
      sum := sum + (c * cnt);
      cnt := cnt - 1;
    end loop;
    return sum;
  end score;

  function score(p1, p2 : in Deck_Vectors.Vector) return Natural is
  begin
    if p1.length = 0 then
      return score(p2);
    else
      return score(p1);
    end if;
  end score;

  procedure take_cards(winner, loser : in out Deck_Vectors.Vector) is
  begin
    winner.append(winner.first_element);
    winner.append(loser.first_element);
    winner.delete(winner.first_index);
    loser.delete(loser.first_index);
  end take_cards;

  procedure battle(player1, player2 : in out Deck_Vectors.Vector) is
  begin
    while player1.length > 0 and player2.length > 0 loop
      if player1.first_element > player2.first_element then
        take_cards(player1, player2);
      else
        take_cards(player2, player1);
      end if;
    end loop;
  end battle;

  function should_recurse(p1, p2 : in Deck_Vectors.Vector) return Boolean is
  begin
    return p1.first_element < Natural(p1.length) and p2.first_element < Natural(p2.length);
  end should_recurse;

  function reduced(d : in Deck_Vectors.Vector) return Deck_Vectors.Vector is
    new_d : Deck_Vectors.Vector := Empty_Vector;
    first : constant Natural := d.first_index + 1;
    last : constant Natural := d.first_index + d.first_element;
  begin
    for idx in first..last loop
      new_d.append(d(idx));
    end loop;
    return new_d;
  end reduced;

  function recursive_battle(player1, player2 : in out Deck_Vectors.Vector) return Boolean is
    past_states : State_Sets.Set := Empty_Set;
  begin
    while player1.length > 0 and player2.length > 0 loop
      declare
        s : constant State := State'(player1 => player1, player2 => player2);
        p1_wins : Boolean;
      begin
        if past_states.contains(s) then
          return true;
        end if;
        past_states.insert(s);

        if should_recurse(player1, player2) then
          declare
            new_p1 : Deck_Vectors.Vector := reduced(player1);
            new_p2 : Deck_Vectors.Vector := reduced(player2);
          begin
            p1_wins := recursive_battle(new_p1, new_p2);
          end;
        else
          p1_wins := player1.first_element > player2.first_element;
        end if;

        if p1_wins then
          take_cards(player1, player2);
        else
          take_cards(player2, player1);
        end if;
      end;
    end loop;
    return player1.length > 0;
  end recursive_battle;

  function combat(filename : in String) return Natural is
    player1 : Deck_Vectors.Vector;
    player2 : Deck_Vectors.Vector;
  begin
    load_decks(filename, player1, player2);
    battle(player1, player2);
    return score(player1, player2);
  end combat;

  function recursive_combat(filename : in String) return Natural is
    player1 : Deck_Vectors.Vector;
    player2 : Deck_Vectors.Vector;
  begin
    load_decks(filename, player1, player2);
    if recursive_battle(player1, player2) then
      TIO.put_line("Player 1 wins");
    else
      TIO.put_line("Player 2 wins");
    end if;
    return score(player1, player2);
  end recursive_combat;
end Day;
