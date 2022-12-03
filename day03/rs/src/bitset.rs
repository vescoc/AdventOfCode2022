const CHARS_SIZE: u8 = (b'z' - b'a' + 1) * 2;

pub struct BitSet<const SIZE: u8>(u64);

pub struct BitSetIter<const SIZE: u8> {
    set: BitSet<SIZE>,
    index: u8,
}

pub type CharBitSet = BitSet<CHARS_SIZE>;
pub type CharBitSetIter = BitSetIter<CHARS_SIZE>;

impl FromIterator<char> for CharBitSet {
    fn from_iter<I: IntoIterator<Item = char>>(i: I) -> Self {
        let mut set = 0;
        for v in i {
            if ('a'..='z').contains(&v) {
                set |= 1 << (v as u8 - b'a')
            } else {
                set |= 1 << (v as u8 - b'A' + (b'z' - b'a' + 1))
            }
        }
        Self(set)
    }
}

impl<const SIZE: u8> BitSet<SIZE> {
    pub fn intersection(&self, other: &Self) -> Self {
        Self(self.0 & other.0)
    }

    fn find_from(&self, mut index: u8) -> u8 {
        while index < SIZE {
            if self.0 & (1 << index) != 0 {
                break;
            }
            index += 1;
        }
        index
    }
}

impl IntoIterator for CharBitSet {
    type Item = <CharBitSetIter as Iterator>::Item;
    type IntoIter = CharBitSetIter;

    fn into_iter(self) -> Self::IntoIter {
        let index = self.find_from(0);
        CharBitSetIter { set: self, index }
    }
}

impl Iterator for CharBitSetIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == (b'z' - b'a' + 1) * 2 {
            None
        } else {
            let value = if self.index < b'z' - b'a' + 1 {
                b'a' + self.index
            } else {
                b'A' + self.index - (b'z' - b'a' + 1)
            };

            self.index += self.set.find_from(self.index + 1);

            Some(value as char)
        }
    }
}
