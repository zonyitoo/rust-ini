//! An insertion-ordered multimap.
//!
//! `keys` stores each distinct key in first-insertion order. `values` stores key indexes and
//! values in value-insertion order.

#[derive(Clone, Default)]
pub(crate) struct OrderedMultimap<Key, Value> {
    keys: Vec<Key>,
    values: Vec<(usize, Value)>,
}

impl<Key, Value> OrderedMultimap<Key, Value> {
    pub(crate) fn clear(&mut self) {
        self.keys.clear();
        self.values.clear();
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    pub(crate) fn iter(&self) -> Iter<'_, Key, Value> {
        Iter {
            keys: &self.keys,
            inner: self.values.iter(),
        }
    }

    pub(crate) fn iter_mut(&mut self) -> IterMut<'_, Key, Value> {
        IterMut {
            keys: &self.keys,
            inner: self.values.iter_mut(),
        }
    }

    pub(crate) fn keys(&self) -> impl DoubleEndedIterator<Item = &Key> + ExactSizeIterator + '_ {
        self.keys.iter()
    }

    pub(crate) fn push_new(&mut self, key: Key, value: Value) -> &mut Value {
        let key_index = self.keys.len();
        self.keys.push(key);
        &mut self.values.push_mut((key_index, value)).1
    }

    pub(crate) fn append_at(&mut self, key_index: usize, value: Value) {
        self.values.push((key_index, value));
    }

    pub(crate) fn first_mut_at(&mut self, key_index: usize) -> Option<&mut Value> {
        self.values
            .iter_mut()
            .find(|(entry_key_index, _)| *entry_key_index == key_index)
            .map(|(_, value)| value)
    }

    pub(crate) fn last_mut_at(&mut self, key_index: usize) -> Option<&mut Value> {
        self.values
            .iter_mut()
            .rfind(|(entry_key_index, _)| *entry_key_index == key_index)
            .map(|(_, value)| value)
    }

    fn remove_key(&mut self, key_index: usize) {
        self.keys.remove(key_index);
        for (entry_key_index, _) in &mut self.values {
            if *entry_key_index > key_index {
                *entry_key_index -= 1;
            }
        }
    }

    fn remove_values(&mut self, key_index: usize) -> Vec<Value> {
        let mut removed = Vec::new();
        // Avoid repeated moves by pushing to a new `Vec` instead.
        let mut retained = Vec::with_capacity(self.values.len());

        for (entry_key_index, value) in std::mem::take(&mut self.values) {
            if entry_key_index == key_index {
                removed.push(value);
            } else {
                retained.push((entry_key_index, value));
            }
        }

        self.values = retained;
        removed
    }

    pub(crate) fn position_key<KeyQuery>(&self, key: &KeyQuery) -> Option<usize>
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        self.keys.iter().position(|entry_key| entry_key == key)
    }

    pub(crate) fn contains_key<KeyQuery>(&self, key: &KeyQuery) -> bool
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        self.position_key(key).is_some()
    }

    pub(crate) fn insert(&mut self, key: Key, value: Value)
    where
        Key: PartialEq,
    {
        match self.position_key(&key) {
            Some(key_index) => {
                self.values.retain(|(entry_key_index, _)| *entry_key_index != key_index);
                self.values.push((key_index, value));
            }
            None => {
                self.push_new(key, value);
            }
        }
    }

    pub(crate) fn append(&mut self, key: Key, value: Value)
    where
        Key: PartialEq,
    {
        match self.position_key(&key) {
            Some(key_index) => self.append_at(key_index, value),
            None => {
                self.push_new(key, value);
            }
        }
    }

    /// Get the first value associated with the key
    pub(crate) fn get<KeyQuery>(&self, key: &KeyQuery) -> Option<&Value>
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        let key_index = self.position_key(key)?;
        self.values
            .iter()
            .find(|(entry_key_index, _)| *entry_key_index == key_index)
            .map(|(_, value)| value)
    }

    /// Get a mutable reference to the first value associated with the key
    pub(crate) fn get_mut<KeyQuery>(&mut self, key: &KeyQuery) -> Option<&mut Value>
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        let key_index = self.position_key(key)?;
        self.values
            .iter_mut()
            .find(|(entry_key_index, _)| *entry_key_index == key_index)
            .map(|(_, value)| value)
    }

    pub(crate) fn get_all<KeyQuery>(&self, key: &KeyQuery) -> impl DoubleEndedIterator<Item = &Value> + '_
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        let key_index = self.position_key(key);
        self.get_all_at(key_index)
    }

    pub(crate) fn get_all_at(&self, key_index: Option<usize>) -> impl DoubleEndedIterator<Item = &Value> + '_ {
        self.values
            .iter()
            .filter(move |(entry_key_index, _)| Some(*entry_key_index) == key_index)
            .map(|(_, value)| value)
    }

    pub(crate) fn get_all_mut<KeyQuery>(&mut self, key: &KeyQuery) -> impl DoubleEndedIterator<Item = &mut Value> + '_
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        let key_index = self.position_key(key);
        self.values
            .iter_mut()
            .filter(move |(entry_key_index, _)| Some(*entry_key_index) == key_index)
            .map(|(_, value)| value)
    }

    /// Remove all values associated with the key and return the first one
    pub(crate) fn remove<KeyQuery>(&mut self, key: &KeyQuery) -> Option<Value>
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        let key_index = self.position_key(key)?;
        let removed = self.remove_values(key_index);
        self.remove_key(key_index);
        removed.into_iter().next()
    }

    pub(crate) fn remove_all<KeyQuery>(&mut self, key: &KeyQuery) -> std::vec::IntoIter<Value>
    where
        Key: PartialEq<KeyQuery>,
        KeyQuery: ?Sized,
    {
        match self.position_key(key) {
            Some(key_index) => {
                let removed = self.remove_values(key_index);
                self.remove_key(key_index);
                removed.into_iter()
            }
            None => Vec::new().into_iter(),
        }
    }
}

impl<Key, Value> PartialEq for OrderedMultimap<Key, Value>
where
    Key: PartialEq,
    Value: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<Key, Value> std::fmt::Debug for OrderedMultimap<Key, Value>
where
    Key: std::fmt::Debug,
    Value: std::fmt::Debug,
{
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_map().entries(self.iter()).finish()
    }
}

pub(crate) struct Iter<'a, Key, Value> {
    keys: &'a [Key],
    inner: std::slice::Iter<'a, (usize, Value)>,
}

impl<'a, Key, Value> Iterator for Iter<'a, Key, Value> {
    type Item = (&'a Key, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(key_index, value)| (&self.keys[*key_index], value))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, Key, Value> DoubleEndedIterator for Iter<'a, Key, Value> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|(key_index, value)| (&self.keys[*key_index], value))
    }
}

pub(crate) struct IterMut<'a, Key, Value> {
    keys: &'a [Key],
    inner: std::slice::IterMut<'a, (usize, Value)>,
}

impl<'a, Key, Value> Iterator for IterMut<'a, Key, Value> {
    type Item = (&'a Key, &'a mut Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(key_index, value)| (&self.keys[*key_index], value))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, Key, Value> DoubleEndedIterator for IterMut<'a, Key, Value> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|(key_index, value)| (&self.keys[*key_index], value))
    }
}

pub(crate) struct IntoIter<Key, Value> {
    keys: Vec<Key>,
    inner: std::vec::IntoIter<(usize, Value)>,
}

impl<Key, Value> Iterator for IntoIter<Key, Value>
where
    Key: Clone,
{
    type Item = (Key, Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(key_index, value)| (self.keys[key_index].clone(), value))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<Key, Value> DoubleEndedIterator for IntoIter<Key, Value>
where
    Key: Clone,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|(key_index, value)| (self.keys[key_index].clone(), value))
    }
}

impl<Key, Value> IntoIterator for OrderedMultimap<Key, Value>
where
    Key: Clone,
{
    type IntoIter = IntoIter<Key, Value>;
    type Item = (Key, Value);

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            keys: self.keys,
            inner: self.values.into_iter(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::OrderedMultimap;

    #[test]
    fn preserves_key_and_value_insertion_order() {
        let mut map = OrderedMultimap::default();
        map.append("a", 1);
        map.append("b", 2);
        map.append("a", 3);

        assert_eq!(map.keys().copied().collect::<Vec<_>>(), ["a", "b"]);
        assert_eq!(
            map.iter().map(|(&key, &value)| (key, value)).collect::<Vec<_>>(),
            [("a", 1), ("b", 2), ("a", 3)]
        );

        map.insert("a", 4);
        assert_eq!(map.keys().copied().collect::<Vec<_>>(), ["a", "b"]);
        assert_eq!(
            map.iter().map(|(&key, &value)| (key, value)).collect::<Vec<_>>(),
            [("b", 2), ("a", 4)]
        );

        assert_eq!(map.remove(&"b"), Some(2));
        assert_eq!(map.iter().next(), Some((&"a", &4)));
    }

    #[test]
    fn debug_matches_the_replaced_multimap() {
        let mut map = OrderedMultimap::default();
        map.append("a", 1);
        map.append("b", 2);
        map.append("a", 3);

        assert_eq!(format!("{map:?}"), r#"{"a": 1, "b": 2, "a": 3}"#);
    }
}
