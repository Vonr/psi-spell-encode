#[macro_use]
extern crate napi_derive;

use std::{
    collections::HashMap,
    io::{BufRead, Cursor, Read},
    ops::Deref,
};

use flate2::read::{GzDecoder, GzEncoder};
use napi::{bindgen_prelude::Utf16String, Status};
use quartz_nbt::{io::Flavor, serde::deserialize_from_buffer};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[napi(constructor)]
pub struct Spell {
    #[serde(rename = "modsRequired")]
    pub mods: Vec<Mod>,
    #[serde(rename = "spellList")]
    pub pieces: Vec<Piece>,
    #[serde(rename = "spellName")]
    pub name: String,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[napi(object)]
pub struct Mod {
    #[serde(rename = "modName")]
    pub name: String,
    #[serde(rename = "modVersion")]
    pub version: String,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[napi(object)]
pub struct Piece {
    pub data: SpellData,
    pub x: u8,
    pub y: u8,
}

const BUILTIN_PARAMS: [&str; 43] = [
    "_target",
    "_number",
    "_number1",
    "_number2",
    "_number3",
    "_number4",
    "_vector1",
    "_vector2",
    "_vector3",
    "_vector4",
    "_position",
    "_min",
    "_max",
    "_power",
    "_x",
    "_y",
    "_z",
    "_radius",
    "_distance",
    "_time",
    "_base",
    "_ray",
    "_vector",
    "_axis",
    "_angle",
    "_pitch",
    "_instrument",
    "_volume",
    "_list1",
    "_list2",
    "_list",
    "_direction",
    "_from1",
    "_from2",
    "_to1",
    "_to2",
    "_root",
    "_toggle",
    "_mask",
    "_channel",
    "_slot",
    "_ray_end",
    "_ray_start",
];

pub type SpellParams = HashMap<String, u8>;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[napi(object)]
pub struct SpellData {
    pub key: String,
    pub params: Option<SpellParams>,
    #[serde(rename = "constant_value")]
    pub constant: Option<String>,
    pub comment: Option<String>,
}

impl Spell {
    #[inline]
    pub fn bin(&self) -> Vec<u8> {
        let mut out: Vec<u8> = Vec::new();
        {
            let name = self.name.as_bytes();
            out.extend_from_slice(name);
            out.push(0);
        }

        for m in &self.mods {
            let name = m.name.as_bytes();
            let version = m.version.as_bytes();
            out.extend_from_slice(name);
            out.push(b',');
            out.extend_from_slice(version);
            out.push(b';');
        }
        let last = out.len() - 1;
        out[last] = b']';

        for piece in &self.pieces {
            let data = &piece.data;
            let key = data.key.as_bytes();
            let key = if &key[0..4] == b"psi:" {
                &key[4..]
            } else {
                key
            };
            let params = &data.params;
            let constant = &data.constant;
            let comment = &data.comment;
            out.push(piece.x << 4 | (piece.y & 0b1111));
            out.extend_from_slice(key);
            out.push(0);
            if let Some(comment) = comment {
                out.extend_from_slice(comment.as_bytes());
            }
            out.push(0);

            if let Some(params) = params {
                out.push(params.len() as u8);
                for (key, side) in params {
                    if let Some(pos) = BUILTIN_PARAMS.iter().position(|e| **e == *key) {
                        out.push(pos as u8);
                    } else {
                        out.push(255);
                        out.extend_from_slice(key.as_bytes());
                        out.push(0);
                    }
                    out.push(*side);
                }
            } else if let Some(constant) = constant {
                out.push(255);
                out.extend_from_slice(constant.as_bytes());
                out.push(0);
            } else {
                out.push(254);
            }
        }

        out
    }

    #[inline]
    pub fn decode(data: &[u8]) -> Self {
        #[inline]
        fn read_until<T>(cursor: &mut Cursor<T>, byte: u8) -> Vec<u8>
        where
            T: std::convert::AsRef<[u8]>,
        {
            let mut out = Vec::new();
            cursor.read_until(byte, &mut out).unwrap();
            out.pop();
            out
        }

        #[inline]
        fn read_until_nul<T>(cursor: &mut Cursor<T>) -> Vec<u8>
        where
            T: std::convert::AsRef<[u8]>,
        {
            read_until(cursor, 0)
        }

        #[inline]
        fn next<T>(cursor: &mut Cursor<T>) -> u8
        where
            T: std::convert::AsRef<[u8]>,
        {
            let mut a = [0];
            cursor.read_exact(&mut a).unwrap();
            a[0]
        }

        #[inline]
        fn btos(b: Vec<u8>) -> String {
            String::from_utf8(b).unwrap()
        }

        let mut cursor = Cursor::new(data);
        let name = btos(read_until_nul(&mut cursor));
        let mut mods = Vec::new();
        let mut pieces = Vec::new();

        {
            let m = read_until(&mut cursor, b']');
            for m in m.split(|b| *b == b';') {
                let mut name = Vec::new();
                let mut version = Vec::new();
                let mut name_done = false;
                for b in m {
                    let b = *b;
                    if b == b',' || b == b';' {
                        name_done = true;
                        continue;
                    }
                    if !name_done {
                        name.push(b);
                    } else {
                        version.push(b);
                    }
                }
                mods.push(Mod {
                    name: btos(name),
                    version: btos(version),
                })
            }
        }

        while cursor.fill_buf().map(|b| !b.is_empty()).unwrap() {
            let xy = next(&mut cursor);
            let x = xy >> 4;
            let y = xy & 0b1111;
            let mut key = read_until_nul(&mut cursor);
            if !key.contains(&b':') {
                key.reserve(4);
                unsafe {
                    std::ptr::copy(key.as_ptr(), key.as_mut_ptr().add(4), key.len());
                    key.set_len(key.len() + 4);
                }
                key[0] = b'p';
                key[1] = b's';
                key[2] = b'i';
                key[3] = b':';
            }
            let key = btos(key);

            let comment = btos(read_until_nul(&mut cursor));
            let comment = if comment.is_empty() {
                None
            } else {
                Some(comment)
            };

            let mut params = HashMap::new();
            let mut constant = None;

            let ty = next(&mut cursor);
            if ty == 255 {
                constant = Some(btos(read_until_nul(&mut cursor)));
            } else if ty != 254 {
                let len = ty;
                for _ in 0..len {
                    let type_or_pos = next(&mut cursor);
                    let param_key = if type_or_pos == 255 {
                        btos(read_until_nul(&mut cursor))
                    } else {
                        BUILTIN_PARAMS[type_or_pos as usize].to_string()
                    };

                    let side = next(&mut cursor);
                    params.insert(param_key, side);
                }
            }

            let params = if params.is_empty() {
                None
            } else {
                Some(params)
            };

            let data = SpellData {
                key,
                params,
                constant,
                comment,
            };

            let piece = Piece { data, x, y };
            pieces.push(piece);
        }

        Self { name, mods, pieces }
    }
}

impl From<&Spell> for Vec<u8> {
    #[inline]
    fn from(value: &Spell) -> Self {
        value.bin()
    }
}

impl<T: Deref<Target = [u8]>> From<T> for Spell {
    #[inline]
    fn from(value: T) -> Self {
        Self::decode(&value)
    }
}

#[napi]
pub fn spell_from_snbt(snbt: String) -> Result<Spell, napi::Error> {
    let snbt =
        quartz_nbt::snbt::parse(&snbt).map_err(|e| napi::Error::new(Status::GenericFailure, e))?;

    let mut bytes = Vec::new();
    if let Err(e) = quartz_nbt::io::write_nbt(&mut bytes, None, &snbt, Flavor::Uncompressed) {
        return Err(napi::Error::new(Status::GenericFailure, e));
    }

    Ok(deserialize_from_buffer(&bytes)
        .map_err(|e| napi::Error::new(Status::GenericFailure, e))?
        .0)
}

#[napi]
pub fn decode_spell_from_bytes(bytes: Vec<u8>) -> Spell {
    bytes.into()
}

#[napi]
pub fn encode_bytes_to_url_safe(bytes: Vec<u8>) -> String {
    const LEVEL: flate2::Compression = flate2::Compression::fast();
    let mut gz = GzEncoder::new(bytes.as_slice(), LEVEL);
    let mut encoded = Vec::new();
    gz.read_to_end(&mut encoded).unwrap();

    base64_simd::URL_SAFE.encode_to_string(encoded)
}

#[napi]
pub fn decode_url_safe_to_bytes(url_safe: String) -> Result<Vec<u8>, napi::Error> {
    let mut bytes = url_safe.into_bytes();
    let decoded = base64_simd::URL_SAFE
        .decode_inplace(&mut bytes)
        .map_err(|e| napi::Error::new(Status::GenericFailure, e))?
        .to_vec();
    let mut gz = GzDecoder::new(&decoded[..]);
    let mut decoded = Vec::new();
    gz.read_to_end(&mut decoded)
        .map_err(|e| napi::Error::new(Status::GenericFailure, e))?;
    Ok(decoded)
}

#[napi]
pub fn encode_spell_to_bytes(spell: &Spell) -> Vec<u8> {
    spell.into()
}

#[napi]
pub fn decode_spell(url_safe: Utf16String) -> Result<Spell, napi::Error> {
    Ok(Spell::decode(&decode_url_safe_to_bytes(
        (*url_safe).to_string(),
    )?))
}

#[napi]
pub fn encode_spell(spell: &Spell) -> Result<Utf16String, napi::Error> {
    Ok(encode_bytes_to_url_safe(encode_spell_to_bytes(spell)).into())
}

#[napi]
pub fn spell_to_snbt(spell: &Spell) -> Result<String, napi::Error> {
    let ser = quartz_nbt::serde::serialize(spell, None, Flavor::Uncompressed).unwrap();
    quartz_nbt::io::read_nbt(&mut Cursor::new(ser), Flavor::Uncompressed)
        .map(|o| o.0.to_snbt())
        .map_err(|e| napi::Error::new(Status::GenericFailure, e))
}
