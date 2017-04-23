-- Copyright 2012-2017 Richard Cobbe
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | This module contains information about the Unicode encoding of polytonic
--   Greek, consisting primarily of definitions for the various relevant
--   Unicode code points, both pure base characters, combining characters, and
--   precomposed base characters.  We omit a few possibilites supported by
--   unicode but not used in applications (tonos, prosgegrammeni).

module Data.Greek.UnicodeData where

-- XXX move allGreekChars to parser, since it's a property of the parser.

import Data.Set (Set)
import qualified Data.Set as Set

-- Combining character definitions

combAcute                  = '\x0301'
combGrave                  = '\x0300'
combCirc                   = '\x0342'
combSmooth                 = '\x0313'
combRough                  = '\x0314'
combIotaSub                = '\x0345'
combDialytika              = '\x0308'
combDieresis               = combDialytika
combMacron                 = '\x0304'
combBreve                  = '\x0306'

-- Base diacriticals
baseDieresis               = '\x00a8'
baseDialytika              = baseDieresis
baseCoronis                = '\x1fbd'
baseIotaSub                = '\x037a'
baseSmooth                 = '\x1fbf'
baseCirc                   = '\x1fc0'
baseCircDialytika          = '\x1fc1'
baseSmoothGrave            = '\x1fcd'
baseSmoothAcute            = '\x1fce'
baseSmoothCirc             = '\x1fcf'
baseRoughGrave             = '\x1fdd'
baseRoughAcute             = '\x1fde'
baseRoughCirc              = '\x1fdf'
baseGraveDialytika         = '\x1fed'
baseAcuteDialytika         = '\x1fee'
baseGrave                  = '\x1fef'
baseAcute                  = '\x1ffd'
baseRough                  = '\x1ffe'

-- bare letters

capAlpha                   = '\x0391'
capBeta                    = '\x0392'
capGamma                   = '\x0393'
capDelta                   = '\x0394'
capEpsilon                 = '\x0395'
capDigamma                 = '\x03dc'
capZeta                    = '\x0396'
capEta                     = '\x0397'
capTheta                   = '\x0398'
capIota                    = '\x0399'
capKappa                   = '\x039a'
capLambda                  = '\x039b'
capMu                      = '\x039c'
capNu                      = '\x039d'
capXi                      = '\x039e'
capOmicron                 = '\x039f'
capPi                      = '\x03a0'
capRho                     = '\x03a1'
capSigma                   = '\x03a3'
capTau                     = '\x03a4'
capUpsilon                 = '\x03a5'
capPhi                     = '\x03a6'
capChi                     = '\x03a7'
capPsi                     = '\x03a8'
capOmega                   = '\x03a9'

baseAlpha                  = '\x03b1'
baseBeta                   = '\x03b2'
baseGamma                  = '\x03b3'
baseDelta                  = '\x03b4'
baseEpsilon                = '\x03b5'
baseDigamma                = '\x03dd'
baseZeta                   = '\x03b6'
baseEta                    = '\x03b7'
baseTheta                  = '\x03b8'
baseIota                   = '\x03b9'
baseKappa                  = '\x03ba'
baseLambda                 = '\x03bb'
baseMu                     = '\x03bc'
baseNu                     = '\x03bd'
baseXi                     = '\x03be'
baseOmicron                = '\x03bf'
basePi                     = '\x03c0'
baseRho                    = '\x03c1'
baseFinalSigma             = '\x03c2'
baseMedialSigma            = '\x03c3'
baseTau                    = '\x03c4'
baseUpsilon                = '\x03c5'
basePhi                    = '\x03c6'
baseChi                    = '\x03c7'
basePsi                    = '\x03c8'
baseOmega                  = '\x03c9'

baseQuestion               = '\x037e'
baseSemicolon              = '\x0387'

-- Precomposed characters
capIotaDialytika           = '\x03aa'
capUpsilonDialytika        = '\x03ab'
iotaDialytika              = '\x03ca'
upsilonDialytika           = '\x03cb'

alphaSmooth                = '\x1f00'
alphaRough                 = '\x1f01'
alphaSmoothGrave           = '\x1f02'
alphaRoughGrave            = '\x1f03'
alphaSmoothAcute           = '\x1f04'
alphaRoughAcute            = '\x1f05'
alphaSmoothCirc            = '\x1f06'
alphaRoughCirc             = '\x1f07'
capAlphaSmooth             = '\x1f08'
capAlphaRough              = '\x1f09'
capAlphaSmoothGrave        = '\x1f0a'
capAlphaRoughGrave         = '\x1f0b'
capAlphaSmoothAcute        = '\x1f0c'
capAlphaRoughAcute         = '\x1f0d'
capAlphaSmoothCirc         = '\x1f0e'
capAlphaRoughCirc          = '\x1f0f'

epsilonSmooth              = '\x1f10'
epsilonRough               = '\x1f11'
epsilonSmoothGrave         = '\x1f12'
epsilonRoughGrave          = '\x1f13'
epsilonSmoothAcute         = '\x1f14'
epsilonRoughAcute          = '\x1f15'
capEpsilonSmooth           = '\x1f18'
capEpsilonRough            = '\x1f19'
capEpsilonSmoothGrave      = '\x1f1a'
capEpsilonRoughGrave       = '\x1f1b'
capEpsilonSmoothAcute      = '\x1f1c'
capEpsilonRoughAcute       = '\x1f1d'

etaSmooth                  = '\x1f20'
etaRough                   = '\x1f21'
etaSmoothGrave             = '\x1f22'
etaRoughGrave              = '\x1f23'
etaSmoothAcute             = '\x1f24'
etaRoughAcute              = '\x1f25'
etaSmoothCirc              = '\x1f26'
etaRoughCirc               = '\x1f27'
capEtaSmooth               = '\x1f28'
capEtaRough                = '\x1f29'
capEtaSmoothGrave          = '\x1f2a'
capEtaRoughGrave           = '\x1f2b'
capEtaSmoothAcute          = '\x1f2c'
capEtaRoughAcute           = '\x1f2d'
capEtaSmoothCirc           = '\x1f2e'
capEtaRoughCirc            = '\x1f2f'

iotaSmooth                 = '\x1f30'
iotaRough                  = '\x1f31'
iotaSmoothGrave            = '\x1f32'
iotaRoughGrave             = '\x1f33'
iotaSmoothAcute            = '\x1f34'
iotaRoughAcute             = '\x1f35'
iotaSmoothCirc             = '\x1f36'
iotaRoughCirc              = '\x1f37'
capIotaSmooth              = '\x1f38'
capIotaRough               = '\x1f39'
capIotaSmoothGrave         = '\x1f3a'
capIotaRoughGrave          = '\x1f3b'
capIotaSmoothAcute         = '\x1f3c'
capIotaRoughAcute          = '\x1f3d'
capIotaSmoothCirc          = '\x1f3e'
capIotaRoughCirc           = '\x1f3f'

omicronSmooth              = '\x1f40'
omicronRough               = '\x1f41'
omicronSmoothGrave         = '\x1f42'
omicronRoughGrave          = '\x1f43'
omicronSmoothAcute         = '\x1f44'
omicronRoughAcute          = '\x1f45'
capOmicronSmooth           = '\x1f48'
capOmicronRough            = '\x1f49'
capOmicronSmoothGrave      = '\x1f4a'
capOmicronRoughGrave       = '\x1f4b'
capOmicronSmoothAcute      = '\x1f4c'
capOmicronRoughAcute       = '\x1f4d'

upsilonSmooth              = '\x1f50'
upsilonRough               = '\x1f51'
upsilonSmoothGrave         = '\x1f52'
upsilonRoughGrave          = '\x1f53'
upsilonSmoothAcute         = '\x1f54'
upsilonRoughAcute          = '\x1f55'
upsilonSmoothCirc          = '\x1f56'
upsilonRoughCirc           = '\x1f57'
capUpsilonRough            = '\x1f59'
capUpsilonRoughGrave       = '\x1f5b'
capUpsilonRoughAcute       = '\x1f5d'
capUpsilonRoughCirc        = '\x1f5f'

omegaSmooth                = '\x1f60'
omegaRough                 = '\x1f61'
omegaSmoothGrave           = '\x1f62'
omegaRoughGrave            = '\x1f63'
omegaSmoothAcute           = '\x1f64'
omegaRoughAcute            = '\x1f65'
omegaSmoothCirc            = '\x1f66'
omegaRoughCirc             = '\x1f67'
capOmegaSmooth             = '\x1f68'
capOmegaRough              = '\x1f69'
capOmegaSmoothGrave        = '\x1f6a'
capOmegaRoughGrave         = '\x1f6b'
capOmegaSmoothAcute        = '\x1f6c'
capOmegaRoughAcute         = '\x1f6d'
capOmegaSmoothCirc         = '\x1f6e'
capOmegaRoughCirc          = '\x1f6f'

alphaGrave                 = '\x1f70'
alphaAcute                 = '\x1f71'
epsilonGrave               = '\x1f72'
epsilonAcute               = '\x1f73'
etaGrave                   = '\x1f74'
etaAcute                   = '\x1f75'
iotaGrave                  = '\x1f76'
iotaAcute                  = '\x1f77'
omicronGrave               = '\x1f78'
omicronAcute               = '\x1f79'
upsilonGrave               = '\x1f7a'
upsilonAcute               = '\x1f7b'
omegaGrave                 = '\x1f7c'
omegaAcute                 = '\x1f7d'

alphaSmoothIotaSub         = '\x1f80'
alphaRoughIotaSub          = '\x1f81'
alphaSmoothGraveIotaSub    = '\x1f82'
alphaRoughGraveIotaSub     = '\x1f83'
alphaSmoothAcuteIotaSub    = '\x1f84'
alphaRoughAcuteIotaSub     = '\x1f85'
alphaSmoothCircIotaSub     = '\x1f86'
alphaRoughCircIotaSub      = '\x1f87'
capAlphaSmoothIotaSub      = '\x1f88'
capAlphaRoughIotaSub       = '\x1f89'
capAlphaSmoothGraveIotaSub = '\x1f8a'
capAlphaRoughGraveIotaSub  = '\x1f8b'
capAlphaSmoothAcuteIotaSub = '\x1f8c'
capAlphaRoughAcuteIotaSub  = '\x1f8d'
capAlphaSmoothCircIotaSub  = '\x1f8e'
capAlphaRoughCircIotaSub   = '\x1f8f'

etaSmoothIotaSub           = '\x1f90'
etaRoughIotaSub            = '\x1f91'
etaSmoothGraveIotaSub      = '\x1f92'
etaRoughGraveIotaSub       = '\x1f93'
etaSmoothAcuteIotaSub      = '\x1f94'
etaRoughAcuteIotaSub       = '\x1f95'
etaSmoothCircIotaSub       = '\x1f96'
etaRoughCircIotaSub        = '\x1f97'
capEtaSmoothIotaSub        = '\x1f98'
capEtaRoughIotaSub         = '\x1f99'
capEtaSmoothGraveIotaSub   = '\x1f9a'
capEtaRoughGraveIotaSub    = '\x1f9b'
capEtaSmoothAcuteIotaSub   = '\x1f9c'
capEtaRoughAcuteIotaSub    = '\x1f9d'
capEtaSmoothCircIotaSub    = '\x1f9e'
capEtaRoughCircIotaSub     = '\x1f9f'

omegaSmoothIotaSub         = '\x1fa0'
omegaRoughIotaSub          = '\x1fa1'
omegaSmoothGraveIotaSub    = '\x1fa2'
omegaRoughGraveIotaSub     = '\x1fa3'
omegaSmoothAcuteIotaSub    = '\x1fa4'
omegaRoughAcuteIotaSub     = '\x1fa5'
omegaSmoothCircIotaSub     = '\x1fa6'
omegaRoughCircIotaSub      = '\x1fa7'
capOmegaSmoothIotaSub      = '\x1fa8'
capOmegaRoughIotaSub       = '\x1fa9'
capOmegaSmoothGraveIotaSub = '\x1faa'
capOmegaRoughGraveIotaSub  = '\x1fab'
capOmegaSmoothAcuteIotaSub = '\x1fac'
capOmegaRoughAcuteIotaSub  = '\x1fad'
capOmegaSmoothCircIotaSub  = '\x1fae'
capOmegaRoughCircIotaSub   = '\x1faf'

alphaBreve                 = '\x1fb0'
alphaMacron                = '\x1fb1'
alphaGraveIotaSub          = '\x1fb2'
alphaIotaSub               = '\x1fb3'
alphaAcuteIotaSub          = '\x1fb4'
alphaCirc                  = '\x1fb6'
alphaCircIotaSub           = '\x1fb7'
capAlphaBreve              = '\x1fb8'
capAlphaMacron             = '\x1fb9'
capAlphaGrave              = '\x1fba'
capAlphaAcute              = '\x1fbb'
capAlphaIotaSub            = '\x1fbc'

etaGraveIotaSub            = '\x1fc2'
etaIotaSub                 = '\x1fc3'
etaAcuteIotaSub            = '\x1fc4'
etaCirc                    = '\x1fc6'
etaCircIotaSub             = '\x1fc7'
capEpsilonGrave            = '\x1fc8'
capEpsilonAcute            = '\x1fc9'
capEtaGrave                = '\x1fca'
capEtaAcute                = '\x1fcb'
capEtaIotaSub              = '\x1fcc'

iotaBreve                  = '\x1fd0'
iotaMacron                 = '\x1fd1'
iotaGraveDialytika         = '\x1fd2'
iotaAcuteDialytika         = '\x1fd3'
iotaCirc                   = '\x1fd6'
iotaCircDialytika          = '\x1fd7'
capIotaBreve               = '\x1fd8'
capIotaMacron              = '\x1fd9'
capIotaGrave               = '\x1fda'
capIotaAcute               = '\x1fdb'

upsilonBreve               = '\x1fe0'
upsilonMacron              = '\x1fe1'
upsilonGraveDialytika      = '\x1fe2'
upsilonAcuteDialytika      = '\x1fe3'
rhoSmooth                  = '\x1fe4'
rhoRough                   = '\x1fe5'
upsilonCirc                = '\x1fe6'
upsilonCircDialytika       = '\x1fe7'
capUpsilonBreve            = '\x1fe8'
capUpsilonMacron           = '\x1fe9'
capUpsilonGrave            = '\x1fea'
capUpsilonAcute            = '\x1feb'
capRhoRough                = '\x1fec'

omegaGraveIotaSub          = '\x1ff2'
omegaIotaSub               = '\x1ff3'
omegaAcuteIotaSub          = '\x1ff4'
omegaCirc                  = '\x1ff6'
omegaCircIotaSub           = '\x1ff7'
capOmicronGrave            = '\x1ff8'
capOmicronAcute            = '\x1ff9'
capOmegaGrave              = '\x1ffa'
capOmegaAcute              = '\x1ffb'
capOmegaIotaSub            = '\x1ffc'

-- | Chars in the Greek Extended Unicode range.  The holes in the ranges
--   correspond to reserved Unicode code points, standalone diacriticals, or
--   letters with a precomposed macron or breve mark (vrachy).
greekExtendedChars :: Set Char
greekExtendedChars =
  Set.unions (
    map Set.fromList [
       ['\x1f00'..'\x1f15'],
       ['\x1f18'..'\x1f1d'],
       ['\x1f20'..'\x1f45'],
       ['\x1f48'..'\x1f4d'],
       ['\x1f50'..'\x1f57'],
       ['\x1f59', '\x1f5b', '\x1f5d', '\x1f5f'],
       ['\x1f60'..'\x1f7d'],
       ['\x1f80'..'\x1faf'],
       ['\x1fb2'..'\x1fb4'],
       ['\x1fb6', '\x1fb7', '\x1fba', '\x1fbb', '\x1fbc'],
       ['\x1fc2', '\x1fc3', '\x1fc4'],
       ['\x1fc6'..'\x1fcc'],
       ['\x1fd2', '\x1fd3', '\x1fd6', '\x1fd7', '\x1fda', '\x1fdb'],
       ['\x1fe2'..'\x1fe7'],
       ['\x1fea'..'\x1fec'],
       ['\x1ff2'..'\x1ff4'],
       ['\x1ff6'..'\x1ffc']])
