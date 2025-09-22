import { readFileSync, writeFileSync } from "fs";
import * as Immutable from "immutable";
import { Either, Option } from "effect";

export class TailCall {
  constructor(
    public readonly fn: any,
    public readonly arg: any,
  ) {}
}

export function $trampoline(val: any) {
  while (true) {
    if (val instanceof TailCall) {
      val = val.fn(val.arg);
    } else {
      return val;
    }
  }
}

type DSLFunction = (...args: any[]) => any;

type DSLU8 = number;
type DSLU16 = number;
type DSLU32 = number;
type DSLU64 = number;
type DSLI8 = number;
type DSLI16 = number;
type DSLI32 = number;
type DSLI64 = number;
type DSLInt = bigint;
type DSLBool = boolean;
type DSLVec<A = any> = A[];
type DSLString = string;
type DSLBytes = Uint8Array<ArrayBuffer>;
type DSLChar = string;

type DSLUnit = [];
type DSLPair<A, B> = [A, B];

type DSLSeq<A = any> = Immutable.List<A>; // NOTE: NOT Immmutable.Seq. See https://immutable-js.com/docs/v5/List/
type DSLArray<A = any> = A[];

type DSLI32MutMap<V = any> = Map<DSLI32, V>;
type DSLI32Map<V = any> = Immutable.Map<DSLI32, V>;

type DSLI32MutSet = Set<DSLI32>;
type DSLI32Set = Immutable.Set<DSLI32>;

type DSLStringMutMap = Map<DSLString, any>;
type DSLStringMap = Immutable.Map<DSLString, any>;
type DSLStringSet = Immutable.Set<DSLString>;

type DSLUnknown = any;

type DSLHostPromise<T = any> = Promise<T>;

type DSLHostEither<L, R> = Either.Either<R, L>;
type DSLHostJSIteratorState<A> =
  | {
      type: "has-peeked-next";
      peekedValue: A;
    }
  | {
      type: "idle";
    }
  | {
      type: "no-more-next";
    };

class DSLHostIterator<A = any> {
  constructor(
    public readonly hasNext: () => DSLBool,
    public readonly next: () => A,
  ) {}

  static fromJSIterator<A>(iterator: Iterator<A>): DSLHostIterator<A> {
    // JavaScript's iterator does not have a 'hasNext()' method, so we need to
    // implement it.

    let state: DSLHostJSIteratorState<A> = { type: "idle" as const };

    const hasNext = (): DSLBool => {
      switch (state.type) {
        case "has-peeked-next":
          return true;
        case "no-more-next":
          return false;
        case "idle": {
          const result = iterator.next();
          if (result.done) {
            state = { type: "no-more-next" };
            return false;
          } else {
            state = { type: "has-peeked-next", peekedValue: result.value };
            return true;
          }
        }
      }
    };

    const next = (): A => {
      switch (state.type) {
        case "has-peeked-next":
          const value = state.peekedValue;
          state = { type: "idle" };
          return value;
        case "no-more-next":
          throw new Error("No more elements in iterator");
        case "idle": {
          const result = iterator.next();
          if (result.done) {
            state = { type: "no-more-next" };
            throw new Error("No more elements in iterator");
          } else {
            state = { type: "idle" };
            return result.value;
          }
        }
      }
    };

    return new DSLHostIterator(hasNext, next);
  }

  static fromJSGenerator<A>(generator: () => Generator<A>): DSLHostIterator<A> {
    return DSLHostIterator.fromJSIterator(generator());
  }
}

class DSLStringBuilder {
  private buffer: string[] = [];

  constructor() {}

  append(str: DSLString) {
    this.buffer.push(str);
  }

  appendChar(char: DSLChar) {
    this.buffer.push(char);
  }

  build() {
    return this.buffer.join("");
  }
}

class DSLRef<A = any> {
  constructor(private value: A) {}

  get(): A {
    return this.value;
  }

  set(value: A) {
    this.value = value;
  }
}

// Basic
export const panic = (str: DSLString) => {
  throw new Error(`[PANIC] ${str}`);
};
export const echo = (value: any) => {
  console.dir(value, { depth: null });
};
export const sys_println = (str: DSLString) => console.log(str);

// Ref
export const ref_create = (init: any): DSLRef => new DSLRef(init);
export const ref_get = (ref: DSLRef) => ref.get();
export const ref_set = (ref: DSLRef) => (value: any) => ref.set(value);

// Vec
export const vec_length = (self: DSLVec) => {
  return self.length;
};
export const vec_at = (self: DSLVec) => (i: DSLI32) => {
  if (i < 0 || i >= self.length) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  return self[i];
};

// Seq
export const seq_nil = Immutable.List<any>() satisfies DSLSeq;
export const seq_is_empty = (self: DSLSeq): DSLBool => {
  return self.isEmpty();
};
export const seq_length = (self: DSLSeq): DSLI32 => {
  return self.size;
};
export const seq_at =
  <T>(self: DSLSeq<T>) =>
  (i: DSLI32): T => {
    if (i < 0 || i >= self.size) {
      throw new Error(`Index out of bounds: ${i}`);
    }
    return self.get(i)!;
  };
export const seq_take =
  (self: DSLSeq) =>
  (length: DSLI32): DSLSeq => {
    return self.take(length);
  };
export const seq_drop =
  (self: DSLSeq) =>
  (length: DSLI32): DSLSeq => {
    return self.slice(length, self.size);
  };
export const seq_take_and_drop =
  (self: DSLSeq) =>
  (length: DSLI32): DSLPair<DSLSeq, DSLSeq> => {
    return [self.take(length), self.slice(length, self.size)] as const;
  };
export const seq_concat =
  (self: DSLSeq) =>
  (other: DSLSeq): DSLSeq => {
    return self.concat(other);
  };
export const seq_cons =
  (self: DSLSeq) =>
  (elem: any): DSLSeq => {
    return self.unshift(elem);
  };
export const seq_snoc =
  (self: DSLSeq) =>
  (elem: any): DSLSeq => {
    return self.push(elem);
  };
export const seq_to_vec = (self: DSLSeq): DSLVec => {
  return self.toArray();
};
export const seq_to_array = (self: DSLSeq): DSLArray => {
  return self.toArray();
};
export const seq_reverse = (self: DSLSeq): DSLSeq => {
  return self.reverse();
};
export const seq_from_vec = (vec: DSLVec): DSLSeq => {
  return Immutable.List(vec);
};
export const seq_create_host_iterator = (self: DSLSeq): DSLHostIterator => {
  return DSLHostIterator.fromJSIterator(self.values());
};
export const seq_create_reversed_host_iterator = (
  self: DSLSeq,
): DSLHostIterator => {
  return DSLHostIterator.fromJSIterator(self.reverse().values());
};
export const seq_inits_host_iterator = (
  self: DSLSeq,
): DSLHostIterator<DSLSeq> => {
  // FIXME: This is NOT O(N) like Haskell's 'inits'
  return DSLHostIterator.fromJSGenerator(function* () {
    for (let i = 0; i <= self.size; i++) {
      yield self.slice(0, i);
    }
  });
};
export const seq_tails_host_iterator = (
  self: DSLSeq,
): DSLHostIterator<DSLSeq> => {
  // FIXME: This is NOT O(N) like Haskell's 'inits'
  return DSLHostIterator.fromJSGenerator(function* () {
    for (let i = 0; i <= self.size; i++) {
      yield self.slice(i, self.size);
    }
  });
};

// Array
export const array_create = (): DSLArray => {
  return [];
};
export const array_set = (self: DSLArray) => (i: DSLI32) => (value: any) => {
  if (i < 0 || i >= self.length) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  self[i] = value;
};
export const array_is_empty = (self: DSLArray): DSLBool => {
  return self.length === 0;
};
export const array_clear = (self: DSLArray) => {
  self.length = 0;
};
export const array_append = (self: DSLArray) => (value: any) => {
  self.push(value);
};
export const array_prepend = (self: DSLArray) => (value: any) => {
  self.unshift(value);
};
export const array_length = (self: DSLArray) => {
  return self.length;
};
export const array_at = (self: DSLArray) => (i: DSLI32) => {
  if (i < 0 || i >= self.length) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  return self[i];
};
export const array_delete = (self: DSLArray) => (i: DSLI32) => {
  if (i < 0 || i >= self.length) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  self.splice(i, 1);
};
export const array_from_vec = (vector: DSLVec) => {
  return [...vector];
};
export const array_to_vec = (array: DSLArray) => {
  return [...array];
};
export const array_to_seq = (array: DSLArray) => {
  return Immutable.List<any>(array);
};

// Bytes
export const bytes_length = (self: DSLBytes) => {
  return self.length;
};
export const bytes_at = (self: DSLBytes) => (i: DSLI32) => {
  if (i < 0 || i >= self.length) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  return self[i];
};

// Char
export const char_equal = (a: DSLChar) => (b: DSLChar) => a === b;
export const char_from_ord = (x: DSLI32) => String.fromCharCode(x);
export const char_to_ord = (c: DSLChar) => c.charCodeAt(0);
export const char_is_space = (c: DSLChar) => c.trim().length === 0;
export const char_is_alpha = (c: DSLChar) => /[a-zA-Z]/.test(c);
export const char_is_alpha_num = (c: DSLChar) => /[a-zA-Z0-9]/.test(c);

// String
export const string_from_array = (arr: DSLArray<DSLChar>) => arr.join("");
export const string_from_char = (char: DSLChar): DSLString => {
  return char;
};
export const string_reverse = (str: DSLString): DSLString => {
  // JS does not have a 'String.prototype.reverse()'?
  let buf = "";
  for (let i = str.length - 1; i >= 0; i--) {
    buf += str[i];
  }
  return buf;
};
export const string_cons = (x: DSLChar) => (str: DSLString) => x + str;
export const string_equal = (a: DSLString) => (b: DSLString) => a === b;
export const string_compare =
  (a: DSLString) =>
  (b: DSLString) =>
  (onLT: any) =>
  (onEQ: any) =>
  (onGT: any) => {
    if (a < b) {
      return onLT;
    } else if (a === b) {
      return onEQ;
    } else {
      return onGT;
    }
  };
export const string_is_empty = (str: DSLString) => str.length === 0;
export const string_length = (str: DSLString) => str.length;
export const string_at = (self: DSLString) => (index: DSLI32) => {
  if (index < 0 || index >= self.length) {
    throw new Error(`Index out of bounds: ${index}`);
  }
  return self[index];
};
export const string_find =
  (self: DSLString) => (start: DSLI32) => (str: DSLString) =>
    self.indexOf(str, start);
export const string_concat = (self: DSLString) => (other: DSLString) =>
  self + other;
export const string_slice =
  (self: DSLString) => (start: DSLI32) => (end: DSLI32) =>
    self.slice(start, end);

// StringBuilder
export const string_builder_create = () => {
  return new DSLStringBuilder();
};
export const string_builder_build = (builder: DSLStringBuilder) =>
  builder.build();
export const string_builder_append_char =
  (builder: DSLStringBuilder) => (char: DSLChar) =>
    builder.appendChar(char);
export const string_builder_append_string =
  (builder: DSLStringBuilder) => (str: DSLString) =>
    builder.append(str);

// I32
export const i32_equal = (a: DSLI32) => (b: DSLI32) => a === b;
export const i32_compare =
  (a: DSLI32) => (b: DSLI32) => (onLT: any) => (onEQ: any) => (onGT: any) => {
    if (a < b) {
      return onLT;
    } else if (a === b) {
      return onEQ;
    } else {
      return onGT;
    }
  };
export const i32_to_str = (x: DSLI32) => String(x);
export const i32_add = (a: DSLI32) => (b: DSLI32) => a + b;
export const i32_sub = (a: DSLI32) => (b: DSLI32) => a - b;
export const i32_mul = (a: DSLI32) => (b: DSLI32) => a * b;
export const i32_floordiv = (a: DSLI32) => (b: DSLI32) => Math.floor(a / b);
export const i32_mod = (a: DSLI32) => (b: DSLI32) => a % b;
export const i32_negate = (a: DSLI32) => -a;

// Int
export const int_equal = (a: DSLInt) => (b: DSLInt) => a === b;
export const int_compare =
  (a: DSLInt) => (b: DSLInt) => (onLT: any) => (onEQ: any) => (onGT: any) => {
    if (a < b) {
      return onLT;
    } else if (a === b) {
      return onEQ;
    } else {
      return onGT;
    }
  };
export const int_to_string = (x: DSLInt) => String(x);
export const int_add = (a: DSLInt) => (b: DSLInt) => a + b;
export const int_sub = (a: DSLInt) => (b: DSLInt) => a - b;
export const int_mul = (a: DSLInt) => (b: DSLInt) => a * b;
export const int_mod = (a: DSLInt) => (b: DSLInt) => a % b;
export const int_negate = (a: DSLInt) => -a;
export const int_clamp_to_i32 = (x: DSLInt) => BigInt.asIntN(32, x);
export const int_from_i32 = (x: DSLI32) => BigInt(x);

// HostEither

export const host_either_unwrap_left = <L, R>(self: DSLHostEither<L, R>): L => {
  return Option.getOrThrow(Either.getLeft(self));
};
export const host_either_unwrap_right = <L, R>(
  self: DSLHostEither<L, R>,
): R => {
  return Option.getOrThrow(Either.getRight(self));
};
export const host_either_is_left = <L, R>(
  self: DSLHostEither<L, R>,
): DSLBool => {
  return Either.isLeft(self);
};
export const host_either_is_right = <L, R>(
  self: DSLHostEither<L, R>,
): DSLBool => {
  return Either.isRight(self);
};

// HostIterator
export const host_iterator_has_next = (self: DSLHostIterator): DSLBool => {
  return self.hasNext();
};
export const host_iterator_next = (self: DSLHostIterator): any => {
  return self.next();
};

// I32MutMap
export const i32_mut_map_create = (): DSLI32MutMap => {
  return new Map();
};
export const i32_mut_map_create_host_iterator = (
  self: DSLI32MutMap,
): DSLHostIterator<DSLPair<DSLI32, any>> => {
  return DSLHostIterator.fromJSIterator(self.entries());
};
export const i32_mut_map_has = (map: DSLI32MutMap) => (key: DSLI32) => {
  return map.has(key);
};
export const i32_mut_map_set =
  (map: DSLI32MutMap) => (key: DSLI32) => (value: any) => {
    map.set(key, value);
  };
export const i32_mut_map_get = (map: DSLI32MutMap) => (key: DSLI32) => {
  if (!map.has(key)) {
    throw new Error(`Key not found in map: ${key}`);
  }
  return map.get(key);
};
export const i32_mut_map_freeze = (self: DSLI32MutMap): DSLI32Map => {
  return Immutable.Map(self);
};

// I32Map
export const i32_map_empty = Immutable.Map<DSLI32, any>() satisfies DSLI32Map;
export const i32_map_has =
  (self: DSLI32Map) =>
  (key: DSLI32): DSLBool => {
    return self.has(key);
  };
export const i32_map_get = (self: DSLI32Map) => (key: DSLI32) => {
  if (!self.has(key)) {
    throw new Error(`Key not found in map: ${key}`);
  }
  return self.get(key);
};
export const i32_map_set =
  (self: DSLI32Map) =>
  (key: DSLI32) =>
  (value: any): DSLI32Map => {
    return self.set(key, value);
  };

// I32MutSet
export const i32_mut_set_create = (): DSLI32MutSet => {
  return new Set<DSLI32>();
};
export const i32_mut_set_is_empty = (self: DSLI32MutSet): DSLBool => {
  return self.size === 0;
};
export const i32_mut_set_add =
  (self: DSLI32MutSet) =>
  (elem: DSLI32): void => {
    self.add(elem);
  };
export const i32_mut_set_has =
  (self: DSLI32MutSet) =>
  (elem: DSLI32): DSLBool => {
    return self.has(elem);
  };
export const i32_mut_set_freeze = (self: DSLI32MutSet): DSLI32Set => {
  return Immutable.Set<DSLI32>(self);
};
export const i32_mut_set_remove = (self: DSLI32MutSet) => (key: DSLI32) => {
  self.delete(key);
};
export const i32_mut_set_choose = (self: DSLI32MutSet): DSLI32 => {
  const value = self.values().next().value;
  if (value === undefined) {
    throw new Error("Set is empty");
  }
  return value;
};
export const i32_mut_set_create_host_iterator = (
  self: DSLI32MutSet,
): DSLHostIterator<DSLI32> => {
  return DSLHostIterator.fromJSIterator(self.values());
};

// I32Set
export const i32_set_empty = Immutable.Set<DSLI32>() satisfies DSLI32Set;
export const i32_set_add =
  (self: DSLI32Set) =>
  (elem: DSLI32): DSLI32Set => {
    return self.add(elem);
  };
export const i32_set_has =
  (self: DSLI32Set) =>
  (elem: DSLI32): DSLBool => {
    return self.has(elem);
  };

// StringMap
export const string_map_empty = Immutable.Map<
  DSLString,
  any
>() satisfies DSLStringMap;
export const string_map_set =
  (self: DSLStringMap) =>
  (key: DSLString) =>
  (value: any): DSLStringMap => {
    return self.set(key, value);
  };
export const string_map_has =
  (self: DSLStringMap) =>
  (key: DSLString): DSLBool => {
    return self.has(key);
  };
export const string_map_get = (self: DSLStringMap) => (key: DSLString) => {
  // NOTE: value could be undefined / null / [] / some other strange values. Use
  // Map.has() instead of Map.get() for now...
  if (!self.has(key)) {
    throw new Error(`Key not found in map: ${key}`);
  }
  return self.get(key);
};
export const string_map_keys_set = (self: DSLStringMap): DSLStringSet => {
  return Immutable.Set(self.keys());
};
export const string_map_create_host_iterator = (
  self: DSLStringMap,
): DSLHostIterator<DSLPair<DSLString, any>> => {
  return DSLHostIterator.fromJSIterator(self.entries());
};

// StringSet
export const string_set_empty = Immutable.Set();
export const string_set_equal =
  (self: DSLStringSet) =>
  (other: DSLStringSet): DSLBool => {
    return self.equals(other);
  };
export const string_set_remove = (self: DSLStringSet) => (elem: DSLString) => {
  return self.remove(elem);
};
export const string_set_has =
  (self: DSLStringSet) =>
  (elem: DSLString): DSLBool => {
    return self.has(elem);
  };
export const string_set_add =
  (self: DSLStringSet) =>
  (elem: DSLString): DSLStringSet => {
    return self.add(elem);
  };
export const string_set_freeze = (self: DSLStringSet): DSLStringSet => {
  return Immutable.Set<DSLString>(self);
};
export const string_set_intersect =
  (self: DSLStringSet) =>
  (other: DSLStringSet): DSLStringSet => {
    return self.intersect(other);
  };
export const string_set_union =
  (self: DSLStringSet) =>
  (other: DSLStringSet): DSLStringSet => {
    return self.union(other);
  };
export const string_set_create_host_iterator = (
  self: DSLStringSet,
): DSLHostIterator<DSLString> => {
  return DSLHostIterator.fromJSIterator(self.keys());
};

// StringMutMap
export const string_mut_map_create = (): DSLStringMutMap => {
  return new Map();
};
export const string_mut_map_freeze = (self: DSLStringMutMap): DSLStringMap => {
  return Immutable.Map<DSLString, any>(self);
};
export const string_mut_map_set =
  (self: DSLStringMutMap) =>
  (key: DSLString) =>
  (value: any): void => {
    self.set(key, value);
  };
export const string_mut_map_has =
  (self: DSLStringMutMap) =>
  (key: DSLString): DSLBool => {
    return self.has(key);
  };
export const string_mut_map_get =
  (self: DSLStringMutMap) => (key: DSLString) => {
    if (!self.has(key)) {
      throw new Error(`Key not found in map: ${key}`);
    }
    return self.get(key);
  };
export const string_mut_map_update =
  (self: DSLStringMutMap) => (other: DSLStringMutMap) => {
    for (const [key, value] of other.entries()) {
      self.set(key, value);
    }
  };

// Unknown
export const unknown_of = (value: any): DSLUnknown => {
  return value;
};

export const unknown_unsafe_to = (value: DSLUnknown): DSLUnknown => {
  return value;
};

// Promise
export const host_promise_then =
  (promise: DSLHostPromise) =>
  (fn: (value: any) => DSLHostPromise): DSLHostPromise => {
    return promise.then((value) => $trampoline(fn(value)));
  };

export const host_promise_of = (value: any): DSLHostPromise => {
  return Promise.resolve(value);
};

// File I/O
export const fs_read_file_utf8 = (
  path: DSLString,
): DSLHostEither<DSLString, DSLString> => {
  try {
    const source = readFileSync(path, "utf8");
    return Either.right(source);
  } catch (err) {
    return Either.left(err instanceof Error ? err.message : "Unknown error");
  }
};
export const fs_write_file_utf8 =
  (path: DSLString) =>
  (content: DSLString): DSLHostEither<DSLString, void> => {
    try {
      writeFileSync(path, content, "utf8");
      return Either.right(undefined);
    } catch (err) {
      return Either.left(err instanceof Error ? err.message : "Unknown error");
    }
  };
