type t('a) = {
  front: list('a),
  frontLen: int,
  rear: list('a),
  rearLen: int,
};
let c = 2;
let empty = {
  /* [1, 2, 3], 1 is the front of the deque */
  front: [],
  frontLen: 0,
  /* [6, 5, 4], 6 is the rear of the deque */
  rear: [],
  rearLen: 0,
};
let make = () => empty;
let isEmpty = deque => deque.frontLen === 0 && deque.rearLen === 0;
let length = deque => deque.frontLen + deque.rearLen;
/*
 * Maintains invariant that `len(front) < c * len(rear) + 1`, and vice-versa.
 */
let maintain = deque =>
  switch (deque) {
  | {front, frontLen, rear, rearLen} when frontLen > c * rearLen + 1 =>
    let i = (frontLen + rearLen) / 2;
    let j = frontLen + rearLen - i;
    let (_, newFrontRev, endOfFrontRev) =
      List.fold_left(
        ((ct, newFront, rest), el) =>
          if (ct < i) {
            (ct + 1, [el, ...newFront], rest);
          } else {
            (ct + 1, newFront, [el, ...rest]);
          },
        (0, [], []),
        front,
      );
    let newFront = List.rev(newFrontRev);
    let newRear = rear @ endOfFrontRev;
    {front: newFront, frontLen: i, rear: newRear, rearLen: j};
  | {front, frontLen, rear, rearLen} when rearLen > c * frontLen + 1 =>
    let i = (frontLen + rearLen) / 2;
    let j = frontLen + rearLen - i;
    let (_, newRearRev, endOfRearRev) =
      List.fold_left(
        ((ct, newRear, rest), el) =>
          if (ct < j) {
            (ct + 1, [el, ...newRear], rest);
          } else {
            (ct + 1, newRear, [el, ...rest]);
          },
        (0, [], []),
        rear,
      );
    let newRear = List.rev(newRearRev);
    let newFront = front @ endOfRearRev;
    {front: newFront, frontLen: i, rear: newRear, rearLen: j};
  | _ => deque
  };

let addFirst = (el: 'a, deque: t('a)): t('a) =>
  maintain({
    ...deque,
    front: [el, ...deque.front],
    frontLen: deque.frontLen + 1,
  });

let getFirstExn = (deque: t('a)): 'a =>
  switch (deque) {
  | {front: [], rear: []} =>
    raise(Exceptions.Empty("DequeCore.getFirstExn"))
  /* Logically impossible for front to be empty and rear to have size 2. */
  | {front: [], rear: [y1, y2, ...rest]} =>
    raise(Exceptions.InternalError("DequeCore.getFirstExn"))
  | {front: [], rear: [y1]} => y1
  | {front: [x1, ...rest]} => x1
  };

let removeFirstExn = (deque: t('a)): t('a) =>
  switch (deque) {
  | {front: [], rear: []} =>
    raise(Exceptions.Empty("DequeCore.removeFirstExn"))
  /* Logically impossible for front to be empty and rear to have size 2. */
  | {front: [], rear: [y1, y2, ...rest]} =>
    raise(Exceptions.InternalError("DequeCore.removeFirstExn"))
  | {front: [], rear: [y1]} => empty
  | {front: [x1, ...rest], frontLen} =>
    maintain({...deque, front: rest, frontLen: frontLen - 1})
  };

let addLast = (el: 'a, deque: t('a)): t('a) =>
  maintain({
    ...deque,
    rear: [el, ...deque.rear],
    rearLen: deque.rearLen + 1,
  });

let getLastExn = (deque: t('a)): 'a =>
  switch (deque) {
  | {front: [], rear: []} => raise(Exceptions.Empty("DequeCore.getLastExn"))
  /* Logically impossible for rear to be empty and front to have size 2. */
  | {front: [x1, x2, ...rest], rear: []} =>
    raise(Exceptions.InternalError("DequeCore.getLastExn"))
  | {front: [x1], rear: []} => x1
  | {rear: [y1, ...rest]} => y1
  };

let removeLastExn = (deque: t('a)): t('a) =>
  switch (deque) {
  | {front: [], rear: []} =>
    raise(Exceptions.Empty("DequeCore.removeLastExn"))
  /* Logically impossible for rear to be empty and front to have size 2. */
  | {front: [x1, x2, ...rest], rear: []} =>
    raise(Exceptions.InternalError("DequeCore.removeLastExn"))
  | {front: [x1], rear: []} => empty
  | {rear: [y1, ...rest], rearLen} =>
    maintain({...deque, rear: rest, rearLen: rearLen - 1})
  };

/* Due to the symmetry of the structure just swap rear and front for reverse. */
let reverse = (deque: t('a)): t('a) => {
  front: deque.rear,
  frontLen: deque.rearLen,
  rear: deque.front,
  rearLen: deque.frontLen,
};

/* Helper to convert from an OCaml list. */
let fromList = (list: list('a)): t('a) =>
  List.fold_left((acc, el) => addLast(el, acc), empty, list);

let toList = (deque: t('el)): list('el) =>
  deque.front @ Caml.List.rev(deque.rear);
