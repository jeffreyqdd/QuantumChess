open State

exception Illegal of string

val measurement : Board.t -> coord -> float Map.Make(Int).t ref -> Board.t
(** [measurement board square] is the board after measurement occurs on
    [square]. We perform measurement as follows:

    Definition: The stability of a tile corresponds to the total percentage
    present on that tile. We say that a piece is stable if the probabilities on
    a tile add up to 100%. We say that a tile is unstable if the probabilities
    add up to below 100%. We say that a tile is super-stable if the probability
    exceeds 100%.

    1. Find the piece that actually exists on [square] out of all
    superpositions. There are two possible cases:

    - First, there is a piece that has 100% probability. Then, we immediately
      say this piece is the piece that exists on the board.
    - Second, no pieces have 100% probability. Then, we choose a random number
      to determine which piece actually exists.

    2. Delete all other probabilities

    - Delete the piece from all tiles except the tile corresponding to [square]
    - Delete all superpositions of the piece except for [square]
    - Delete all unused probability credits in the bank account

    3. For all other superposition pieces on [square]

    - Take the piece's probability p and add it to a bank account. The bank
      account holds probability credits, which is probability that does not
      appear on the board.
    - From the bank account, while there still exists probability credits for
      the piece, attempt to evenly spread all probability credits in the account
      to all remaining superpositions of the piece. An attempt has three
      possible outcomes:
    - First, the attempt succeeds if the stability of the tile does not exceed
      100%.
    - Second, the attempt succeeds if adding the probability credits causes a
      piece's probability on that tile to equal 100%. The tile is measured
      immediately.
    - Third, the attempt is blocked if the tile becomes super-stable without any
      specific piece to have a probability equal to 100%. In this case, we
      measure the tile immediately and end this iteration of the while loop. *)
