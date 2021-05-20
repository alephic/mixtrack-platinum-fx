
MixtrackPlatinumFX = {};

/*
    Mixtrack Platinum FX mapping
    Author: Kaj Bostrom

    Notable nonstandard features:
    - HPF & LPF FX buttons remapped to autopan / bitcrush
    - Shift + FX buttons selects focused FX unit + slot
    - "Fader cuts" pad mode remapped to beatjump
    - Shift + Hotcue mapped to cueloop (from Virtual DJ) (not implemented yet)
    - Shift + Sample mapped to key cue / pitch play (from Virtual DJ / Serato DJ Pro) (not implemented yet)
    - Shift + Play starts/ends slip
    - Shift + Sync toggles quantize

    Scratch & nudge code + jogwheel display code adapted from the Mixtrack Pro 3 mapping
    by Stéphane Morin & Radu Suciu and the Mixtrack Platinum mapping by Matthew Nicholson
*/

// Scratch parameters
var intervalsPerRev = 1000;
var rpm = 33 + 1 / 3;
var alpha = 1.0 / 8;  
var beta = alpha / 32;

MixtrackPlatinumFX.rateRangeOptions = [0.08, 0.15, 0.5];

// Pad config
MixtrackPlatinumFX.autoloopSizes = [
    '0.0625', '0.125','0.25', '0.5',
    '1', '2', '4', '8'
];
MixtrackPlatinumFX.beatjumpSizes = [
    -2, -1, 1, 2,
    -8, -4, 4, 8,
];
MixtrackPlatinumFX.beatjumpSizesShift = [
    -16, -8, 8, 16,
    -64, -32, 32, 64
];

// How many jog ticks correspond to a beatjump in fast seek mode
MixtrackPlatinumFX.fastSeekJogTicks = 4;
// How many beats to jump at a time in fast seek mode
MixtrackPlatinumFX.fastSeekRate = 4;

// Helper functions

var sendSysex = function(buffer) {
    midi.sendSysexMsg(buffer, buffer.length);
}

MixtrackPlatinumFX.encodeNumToArray = function(number, drop, unsigned) {
    var number_array = [
        (number >> 28) & 0x0F,
        (number >> 24) & 0x0F,
        (number >> 20) & 0x0F,
        (number >> 16) & 0x0F,
        (number >> 12) & 0x0F,
        (number >> 8) & 0x0F,
        (number >> 4) & 0x0F,
        number & 0x0F,
    ];

    if (drop !== undefined) {
        number_array.splice(0, drop);
    }

    if (number < 0) number_array[0] = 0x07;
    else if (!unsigned) number_array[0] = 0x08;

    return number_array;
};

MixtrackPlatinumFX.sendScreenRateMidi = function(deck, rate) {
    rateArray = MixtrackPlatinumFX.encodeNumToArray(rate, 2);

    var bytePrefix = [0xF0, 0x00, 0x20, 0x7F, deck, 0x02];
    var bytePostfix = [0xF7];
    var byteArray = bytePrefix.concat(rateArray, bytePostfix);
    sendSysex(byteArray);
};

MixtrackPlatinumFX.sendScreenTimeMidi = function(deck, time) {
    var timeArray = MixtrackPlatinumFX.encodeNumToArray(time);

    var bytePrefix = [0xF0, 0x00, 0x20, 0x7F, deck, 0x04];
    var bytePostfix = [0xF7];
    var byteArray = bytePrefix.concat(timeArray, bytePostfix);
    sendSysex(byteArray);
};

MixtrackPlatinumFX.sendScreenBpmMidi = function(deck, bpm) {
    bpmArray = MixtrackPlatinumFX.encodeNumToArray(bpm, 2, true);

    var bytePrefix = [0xF0, 0x00, 0x20, 0x7F, deck, 0x01];
    var bytePostfix = [0xF7];
    var byteArray = bytePrefix.concat(bpmArray, bytePostfix);
    sendSysex(byteArray);
};

MixtrackPlatinumFX.bpmCallback = function(value, group, control) {
    MixtrackPlatinumFX.sendScreenBpmMidi(MixtrackPlatinumFX.groupChannels[group]+1, value*100);
};

// [channel offset, ctrl]
MixtrackPlatinumFX.genericLEDs = {
    "cue_indicator": [0, 0x01],
    "start": [0, 0x05],
    "play_indicator": [0, 0x00],
    "slip_enabled": [0, 0x04],
    "quantize": [0, 0x03],
    "sync_enabled": [0, 0x02],
    "pfl": [0, 0x1b],
    "loop_enabled": [4, 0x40],
    "reloop_toggle": [4, 0x41],
    "loop_halve": [4, 0x34],
    "loop_double": [4, 0x35],
    "loop_in": [4, 0x36],
    "loop_out": [4, 0x37]
};

MixtrackPlatinumFX.binaryLEDs = {
    "keylock": [0, 0x0d]
};

MixtrackPlatinumFX.genericLEDCallback = function(value, group, control) {
    var channel = MixtrackPlatinumFX.groupChannels[group];
    var LEDMapEntry = MixtrackPlatinumFX.genericLEDs[control];
    midi.sendShortMsg(0x90 + channel+LEDMapEntry[0], LEDMapEntry[1], 2 + 0x7d*value);
};

MixtrackPlatinumFX.binaryLEDCallback = function(value, group, control) {
    var channel = MixtrackPlatinumFX.groupChannels[group];
    var LEDMapEntry = MixtrackPlatinumFX.binaryLEDs[control];
    midi.sendShortMsg(0x90 + channel+LEDMapEntry[0], LEDMapEntry[1], value);
};

MixtrackPlatinumFX.positionCallback = function(position, group, control) {

    var channel = MixtrackPlatinumFX.groupChannels[group];
    var pos = Math.round(position * 52);
    if (pos < 0) {
        pos = 0;
    }
    midi.sendShortMsg(0xB0 | channel, 0x3F, pos);

    // get the current duration
    duration = engine.getValue(group, "duration");
    var timeElapsed = duration * position;

    // update the time display
    MixtrackPlatinumFX.sendScreenTimeMidi(channel+1, Math.round(timeElapsed * 1000));

    // update the spinner (range 64-115, 52 values)
    //
    // the visual spinner in the mixxx interface takes 1.8 seconds to loop
    // (60 seconds/min divided by 33 1/3 revolutions per min)
    var period = 60 / (33+1/3);
    var midiResolution = 52; // the controller expects a value range of 64-115
    var spinner = Math.round(timeElapsed % period * (midiResolution / period));
    if (spinner < 0) {
        spinner += 115;
    } else {
        spinner += 64;
    }

    midi.sendShortMsg(0xB0 | channel, 0x06, spinner);
};

MixtrackPlatinumFX.rateCallback = function(rate, group, control)  {
    var channel = MixtrackPlatinumFX.groupChannels[group];
    var rateEffective = engine.getValue(group, "rateRange") * rate;
    MixtrackPlatinumFX.sendScreenRateMidi(channel+1, Math.round(rateEffective*10000));
};

MixtrackPlatinumFX.cycleRateRange = function(channel, control, value, status, group) {
    var currRangeIdx = MixtrackPlatinumFX.deckRateRangeIdxs[channel];
    var newRangeIdx = (currRangeIdx + 1) % MixtrackPlatinumFX.rateRangeOptions.length;
    MixtrackPlatinumFX.deckRateRangeIdxs[channel] = newRangeIdx;
    newRange = MixtrackPlatinumFX.rateRangeOptions[newRangeIdx];
    MixtrackPlatinumFX.updateRateRange(channel, newRange);
};

MixtrackPlatinumFX.updateRateRange = function(channel, range) {
    var group = MixtrackPlatinumFX.deckGroups[channel];
    engine.setParameter(group, "rateRange", (range-0.01)*0.25);
    midi.sendShortMsg(0x90+channel, 0x0e, range*100);
};

MixtrackPlatinumFX.beatloopToggle = function(channel, control, value, status, group) {
    if (engine.getParameter(group, "loop_enabled")) {
        engine.setParameter(group, "reloop_toggle", 1);
    } else {
        engine.setParameter(group, "beatloop_activate", 1);
    }
};

MixtrackPlatinumFX.shift = function(channel, control, value, status, group) {
    MixtrackPlatinumFX.shifted = value == 0x7f;
};

MixtrackPlatinumFX.toggleScratch = function(channel, control, value, status, group) {
    MixtrackPlatinumFX.deckScratchModes[channel] = !MixtrackPlatinumFX.deckScratchModes[channel];
    midi.sendShortMsg(0x90+channel, 0x07, MixtrackPlatinumFX.deckScratchModes[channel] ? 0x7f : 0x00);
};

MixtrackPlatinumFX.wheelStopCheck = function(deck) {
    if (MixtrackPlatinumFX.deckLastScratchTicks[deck] > 2
        || MixtrackPlatinumFX.deckLastScratchTicks[deck] < -1) {
        MixtrackPlatinumFX.deckLastScratchTicks[deck] = 0;
    } else {
        if (MixtrackPlatinumFX.deckScratchStates[deck] == 1) {
            engine.scratchDisable(deck+1, true);
        }
        MixtrackPlatinumFX.deckScratchStates[deck] = 0;
        engine.stopTimer(MixtrackPlatinumFX.deckWheelStopTimers[deck]);
        MixtrackPlatinumFX.deckWheelStopTimers[deck] = 0;
        MixtrackPlatinumFX.deckFastSeekAccums[deck] = 0;
    }
}

MixtrackPlatinumFX.touchWheel = function(channel, control, value, status, group) {
    if (value == 0x7f) {
        if (MixtrackPlatinumFX.deckWheelStopTimers[channel]) {
            engine.stopTimer(MixtrackPlatinumFX.deckWheelStopTimers[channel]);
            MixtrackPlatinumFX.deckWheelStopTimers[channel] = 0;
        }
        if (MixtrackPlatinumFX.shifted) {
            // fast seek mode
            MixtrackPlatinumFX.deckScratchStates[channel] = 2;
        } else if (MixtrackPlatinumFX.deckScratchModes[channel] || !engine.getValue(group, "play")) {
            // scratch mode
            MixtrackPlatinumFX.deckScratchStates[channel] = 1;
            engine.scratchEnable(channel+1, intervalsPerRev, rpm, alpha, beta);
        }
    } else {
        MixtrackPlatinumFX.deckWheelStopTimers[channel] = engine.beginTimer(20, function() {
            MixtrackPlatinumFX.wheelStopCheck(channel);
        }, false);
    }
};

MixtrackPlatinumFX.jog = function(channel, control, value, status, group) {
    var amount = value > 63 ? value - 128 : value;
    MixtrackPlatinumFX.deckLastScratchTicks[channel] = amount;
    if (MixtrackPlatinumFX.deckScratchStates[channel] == 1) {
        engine.scratchTick(channel+1, amount);
    } else if (MixtrackPlatinumFX.deckScratchStates[channel] == 2 || MixtrackPlatinumFX.shifted) {
        MixtrackPlatinumFX.deckFastSeekAccums[channel] += amount;
        if (Math.abs(MixtrackPlatinumFX.deckFastSeekAccums[channel]) > MixtrackPlatinumFX.fastSeekJogTicks) {
            var seekAmount = Math.floor(Math.abs(MixtrackPlatinumFX.deckFastSeekAccums[channel])/MixtrackPlatinumFX.fastSeekJogTicks);
            var seekDir = MixtrackPlatinumFX.deckFastSeekAccums[channel] < 0 ? -1 : 1;
            engine.setValue(group, "beatjump", MixtrackPlatinumFX.fastSeekRate*seekAmount*seekDir);
            MixtrackPlatinumFX.deckFastSeekAccums[channel] -= seekAmount*seekDir*MixtrackPlatinumFX.fastSeekJogTicks;
        }
    } else {
        var gammaInputRange = 13; // Max jog speed
        var maxOutFraction = 0.8; // Where on the curve it should peak; 0.5 is half-way
        var sensitivity = 0.5; // Adjustment gamma
        var gammaOutputRange = 0.75; // Max rate change

        var nudge = (amount < 0 ? -1 : 1) * gammaOutputRange * Math.pow(
            Math.abs(amount) / (gammaInputRange * maxOutFraction),
            sensitivity
        );

        engine.setValue(group, "jog", nudge);
    }
};

MixtrackPlatinumFX.instantDoublesTimeout = function() {
    MixtrackPlatinumFX.instantDoubles = false;
};

MixtrackPlatinumFX.loadTrack = function(channel, control, value, status, group) {
    if (value == 0x7f) {
        if (MixtrackPlatinumFX.shifted) {
            var otherDeck = MixtrackPlatinumFX.activeDecks[1-(MixtrackPlatinumFX.groupChannels[group]%2)];
            engine.setValue(group, "CloneFromDeck", 1+otherDeck);
        } else {
            if (MixtrackPlatinumFX.instantDoubles) {
                MixtrackPlatinumFX.instantDoubles = false;
                engine.stopTimer(MixtrackPlatinumFX.instantDoublesTimeoutID);
                var otherDeck = MixtrackPlatinumFX.activeDecks[1-(MixtrackPlatinumFX.groupChannels[group]%2)];
                engine.setValue(MixtrackPlatinumFX.deckGroups[otherDeck], "LoadSelectedTrack", 1);
            } else {
                engine.setValue(group, "LoadSelectedTrack", 1);
                MixtrackPlatinumFX.instantDoubles = true;
                MixtrackPlatinumFX.instantDoublesTimeoutID = engine.beginTimer(500, MixtrackPlatinumFX.instantDoublesTimeout, true);
            }
        }
    }
};

MixtrackPlatinumFX.switchActiveDeck = function(channel, control, value, status, group) {
    MixtrackPlatinumFX.activeDecks[channel%2] = channel;
};

MixtrackPlatinumFX.vuCallback = function(value, group, control) {
    var channel = MixtrackPlatinumFX.groupChannels[group];
    var level = Math.round(value*80);
    if (engine.getValue(group, "PeakIndicator")) {
        level = 81;
    }
    midi.sendShortMsg(0xb0+channel, 0x1f, level);
};

MixtrackPlatinumFX.padModeMap = {
    0x00: 0, // hotcue 1
    0x0D: 1, // loop
    0x07: 2, // "fader cuts" - beatjump
    0x0B: 3, // sample
    0x02: 4, // cueloop
    0x0F: 5, // key cue/pitch play
};

MixtrackPlatinumFX.padModeAddrs = [0x00, 0x0D, 0x07, 0x0B, 0x02, 0x0F];

MixtrackPlatinumFX.altPadModes = {
    4: 0,
    5: 3
};

MixtrackPlatinumFX.getHotcueActivateCallback = function(channel, pad) {
    return function(value, group, control) {
        if (MixtrackPlatinumFX.deckPadModes[channel] == 0) {
            var brightness = value ? 0x7f : engine.getValue(group, 'hotcue_'+(pad+1)+'_enabled')*0x02;
            midi.sendShortMsg(0x94+channel, 0x14+pad, brightness);
        }
    };
};

MixtrackPlatinumFX.getHotcueEnabledCallback = function(channel, pad) {
    return function(value, group, control) {
        if (MixtrackPlatinumFX.deckPadModes[channel] == 0) {
            var brightness = value*0x02;
            if (!engine.getValue(group, 'hotcue_'+(pad+1)+'_activate')) {
                midi.sendShortMsg(0x94+channel, 0x14+pad, brightness);
            }
            midi.sendShortMsg(0x94+channel, 0x1C+pad, brightness);
        }
    };
}

MixtrackPlatinumFX.makePadHotcueLEDConnections = function(channel) {
    var deckGroup = MixtrackPlatinumFX.deckGroups[channel];
    for (var pad = 0; pad < 8; ++pad) {
        var callbackActivate = MixtrackPlatinumFX.getHotcueActivateCallback(channel, pad);
        var callbackEnabled = MixtrackPlatinumFX.getHotcueEnabledCallback(channel, pad);
        engine.makeConnection(deckGroup, 'hotcue_'+(pad+1)+'_activate', callbackActivate);
        var connectionEnabled = engine.makeConnection(deckGroup, 'hotcue_'+(pad+1)+'_enabled', callbackEnabled);
        MixtrackPlatinumFX.deckPadLEDConnections[channel][0].push(connectionEnabled);
    }
};

MixtrackPlatinumFX.getLoopEnabledCallback = function(channel, pad) {
    return function(value, group, control) {
        if (MixtrackPlatinumFX.deckPadModes[channel] == 1) {
            var brightness = value*0x7d + 0x02;
            midi.sendShortMsg(0x94+channel, 0x14+pad, brightness);
            midi.sendShortMsg(0x94+channel, 0x1C+pad, brightness);
        }
    };
};

MixtrackPlatinumFX.makePadAutoloopLEDConnections = function(channel) {
    var deckGroup = MixtrackPlatinumFX.deckGroups[channel];
    for (var pad = 0; pad < 8; ++pad) {
        var loopSize = MixtrackPlatinumFX.autoloopSizes[pad];
        var loopEnabled = 'beatloop_'+loopSize+'_enabled';
        var callbackEnabled = MixtrackPlatinumFX.getLoopEnabledCallback(channel, pad);
        var connectionEnabled = engine.makeConnection(deckGroup, loopEnabled, callbackEnabled);
        MixtrackPlatinumFX.deckPadLEDConnections[channel][1].push(connectionEnabled);
    }
};

MixtrackPlatinumFX.getBeatjumpCallback = function(channel, pad) {
    return function(value, group, control) {
        if (MixtrackPlatinumFX.deckPadModes[channel] == 2) {
            var brightness = value * 0x7d + 0x02;
            midi.sendShortMsg(0x94+channel, 0x14+pad, brightness);
        }
    };
};

MixtrackPlatinumFX.makePadBeatjumpLEDConnections = function(channel) {
    var deckGroup = MixtrackPlatinumFX.deckGroups[channel];
    for (var pad = 0; pad < 16; ++pad) {
        var jumpSize = pad < 8 ? MixtrackPlatinumFX.beatjumpSizes[pad] : MixtrackPlatinumFX.beatjumpSizesShift[pad-8];
        var jumpControl = null;
        if (jumpSize < 0) {
            jumpControl = 'beatjump_'+(-jumpSize)+'_backward';
        } else {
            jumpControl = 'beatjump_'+jumpSize+'_forward';
        }
        var callback = MixtrackPlatinumFX.getBeatjumpCallback(channel, pad);
        var connection = engine.makeConnection(deckGroup, jumpControl, callback);
        MixtrackPlatinumFX.deckPadLEDConnections[channel][2].push(connection);
    }
};

MixtrackPlatinumFX.getSamplerPlayCallback = function(channel, pad) {
    var samplerGroup = '[Sampler'+(pad+1)+']';
    return function(value, group, control) {
        if (MixtrackPlatinumFX.deckPadModes[channel] == 3) {
            var brightness = engine.getValue(samplerGroup, 'track_loaded') * (value*0x7d + 0x02);
            midi.sendShortMsg(0x94+channel, 0x14+pad, brightness);
            midi.sendShortMsg(0x94+channel, 0x1C+pad, brightness);
        }
    };
};

MixtrackPlatinumFX.getSamplerLoadedCallback = function(channel, pad) {
    return function(value, group, control) {
        if (MixtrackPlatinumFX.deckPadModes[channel] == 3) {
            var brightness = value*0x02;
            midi.sendShortMsg(0x94+channel, 0x14+pad, brightness);
            midi.sendShortMsg(0x94+channel, 0x1C+pad, brightness);
        }
    };
};

MixtrackPlatinumFX.makePadSamplerLEDConnections = function(channel) {
    for (var pad = 0; pad < 8; ++pad) {
        var samplerGroup = '[Sampler'+(pad+1)+']';
        var callbackPlay = MixtrackPlatinumFX.getSamplerPlayCallback(channel, pad);
        var callbackLoaded = MixtrackPlatinumFX.getSamplerLoadedCallback(channel, pad);
        var connectionPlay = engine.makeConnection(samplerGroup, 'play', callbackPlay);
        engine.makeConnection(samplerGroup, 'track_loaded', callbackLoaded);
        MixtrackPlatinumFX.deckPadLEDConnections[channel][3].push(connectionPlay);
    };
};


MixtrackPlatinumFX.updatePadModeLED = function(channel, padMode) {
    var addr = MixtrackPlatinumFX.padModeAddrs[padMode];
    var modeActive = padMode == MixtrackPlatinumFX.deckPadModes[channel]
        || (padMode === MixtrackPlatinumFX.altPadModes[MixtrackPlatinumFX.deckPadModes[channel]]
            && MixtrackPlatinumFX.blinkState);
    var brightness = modeActive*0x7d + 0x02;
    midi.sendShortMsg(0x94 + channel, addr, brightness);
};

MixtrackPlatinumFX.blinkCallback = function() {
    MixtrackPlatinumFX.blinkState = 1 - MixtrackPlatinumFX.blinkState;
    for (var deck = 0; deck < 4; ++deck) {
        var altMode = MixtrackPlatinumFX.altPadModes[MixtrackPlatinumFX.deckPadModes[deck]];
        if (altMode !== undefined) {
            var brightness = MixtrackPlatinumFX.blinkState*0x7d + 0x02;
            midi.sendShortMsg(0x94+deck, MixtrackPlatinumFX.padModeAddrs[altMode], brightness);
        }
    }
    MixtrackPlatinumFX.updateFXButtonLED(MixtrackPlatinumFX.activeFXSlot);
};

MixtrackPlatinumFX.updatePadLEDs = function(deck) {
    var padMode = MixtrackPlatinumFX.deckPadModes[deck];
    for (var i = 0; i < MixtrackPlatinumFX.deckPadLEDConnections[deck][padMode].length; ++i) {
        MixtrackPlatinumFX.deckPadLEDConnections[deck][padMode][i].trigger();
    }
};

MixtrackPlatinumFX.switchPadMode = function(channel, control, value, status, group) {
    var deck = channel - 4;
    var oldPadMode = MixtrackPlatinumFX.deckPadModes[deck];
    var oldAltPadMode = MixtrackPlatinumFX.altPadModes[oldPadMode];
    var newPadMode = MixtrackPlatinumFX.padModeMap[control];
    MixtrackPlatinumFX.deckPadModes[deck] = newPadMode;
    MixtrackPlatinumFX.updatePadModeLED(deck, oldPadMode);
    if (oldAltPadMode !== undefined) {
        MixtrackPlatinumFX.updatePadModeLED(deck, oldAltPadMode);
    }
    MixtrackPlatinumFX.updatePadModeLED(deck, newPadMode);
    MixtrackPlatinumFX.updatePadLEDs(deck);
};

MixtrackPlatinumFX.hotcuePad = function(channel, pad, value) {
    engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'hotcue_'+(pad+1)+'_activate', value);
};
MixtrackPlatinumFX.hotcuePadShifted = function(channel, pad, value) {
    engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'hotcue_'+(pad+1)+'_clear', value);
};

MixtrackPlatinumFX.autoloopPad = function(channel, pad, value) {
    engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'beatloop_'+MixtrackPlatinumFX.autoloopSizes[pad]+'_toggle', value);
};
MixtrackPlatinumFX.autoloopPadShifted = function(channel, pad, value) {
    engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'beatlooproll_'+MixtrackPlatinumFX.autoloopSizes[pad]+'_activate', value);
};

MixtrackPlatinumFX.beatjumpPad = function(channel, pad, value) {
    var amount = MixtrackPlatinumFX.beatjumpSizes[pad];
    if (amount < 0) {
        engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'beatjump_'+(-amount)+'_backward', value);
    } else {
        engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'beatjump_'+amount+'_forward', value);
    }
};

MixtrackPlatinumFX.beatjumpPadShifted = function(channel, pad, value) {
    var amount = MixtrackPlatinumFX.beatjumpSizesShift[pad];
    if (amount < 0) {
        engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'beatjump_'+(-amount)+'_backward', value);
    } else {
        engine.setValue(MixtrackPlatinumFX.deckGroups[channel], 'beatjump_'+amount+'_forward', value);
    }
};

MixtrackPlatinumFX.samplerPad = function(channel, pad, value) {
    engine.setValue('[Sampler'+(pad+1)+']', 'start_play', value);
};

MixtrackPlatinumFX.padFunctions = [
    MixtrackPlatinumFX.hotcuePad,
    MixtrackPlatinumFX.autoloopPad,
    MixtrackPlatinumFX.beatjumpPad,
    MixtrackPlatinumFX.samplerPad
];

MixtrackPlatinumFX.padFunctionsShifted = [
    MixtrackPlatinumFX.hotcuePadShifted,
    MixtrackPlatinumFX.autoloopPadShifted,
    MixtrackPlatinumFX.beatjumpPadShifted,
    MixtrackPlatinumFX.samplerPad
];

MixtrackPlatinumFX.pad = function(channel, control, value, status, group) {
    var deck = channel - 4;
    var pad = control - 0x14;
    value = value == 0x7f ? 1 : 0;
    if (pad < 8) {
        MixtrackPlatinumFX.padFunctions[MixtrackPlatinumFX.deckPadModes[deck]](deck, pad, value);
    } else {
        MixtrackPlatinumFX.padFunctionsShifted[MixtrackPlatinumFX.deckPadModes[deck]](deck, pad-8, value);
    }
};

// Which hardware button labels correspond to which Mixxx FX indices
MixtrackPlatinumFX.fxModeMap = [
    1, // HPF -> Autopan
    5, // LPF -> Bitcrush
    10, // Flanger
    8, // Echo
    18, // Reverb
    17 // Phaser
];

MixtrackPlatinumFX.fxBeatsCallbackDefault = function(effectGroup, knobInput) {
    var oldValue = engine.getParameter(effectGroup, 'parameter1');
    var newValue = knobInput == 0x01 ? oldValue+0.125 : oldValue-0.125;
    engine.setParameter(effectGroup, 'parameter1', newValue);
};

MixtrackPlatinumFX.fxBeatsCallbackBitcrush = function(effectGroup, knobInput) {
};

MixtrackPlatinumFX.fxBeatsCallbacks = [
    MixtrackPlatinumFX.fxBeatsCallbackDefault,
    MixtrackPlatinumFX.fxBeatsCallbackBitcrush,
    MixtrackPlatinumFX.fxBeatsCallbackDefault,
    MixtrackPlatinumFX.fxBeatsCallbackDefault,
    MixtrackPlatinumFX.fxBeatsCallbackDefault,
    MixtrackPlatinumFX.fxBeatsCallbackDefault
];

MixtrackPlatinumFX.getActiveEffectUnitGroup = function() {
    return "[EffectRack1_EffectUnit"+(MixtrackPlatinumFX.activeFXSlot < 3 ? 1 : 2)+']';
};

MixtrackPlatinumFX.getActiveEffectGroup = function() {
    return "[EffectRack1_EffectUnit"+(MixtrackPlatinumFX.activeFXSlot < 3 ? 1 : 2)+"_Effect"+((MixtrackPlatinumFX.activeFXSlot % 3) + 1)+']';
};

MixtrackPlatinumFX.fxMix = function(channel, control, value, status, group) {
    engine.setValue(MixtrackPlatinumFX.getActiveEffectUnitGroup(), 'mix', value/127);
};

MixtrackPlatinumFX.fxBeats = function(channel, control, value, status, group) {
    var effectGroup = MixtrackPlatinumFX.getActiveEffectGroup();
    if (engine.getValue(effectGroup, 'loaded')) {
        MixtrackPlatinumFX.fxBeatsCallbacks[MixtrackPlatinumFX.fxModes[MixtrackPlatinumFX.activeFXSlot]](effectGroup, value);
    }
};

MixtrackPlatinumFX.fxButton = function(channel, control, value, status, group) {
    if (value == 0x7f) {
        if (MixtrackPlatinumFX.shifted) {
            var oldFXSlot = MixtrackPlatinumFX.activeFXSlot;
            MixtrackPlatinumFX.activeFXSlot = control;
            MixtrackPlatinumFX.updateFXButtonLED(oldFXSlot);
            if (MixtrackPlatinumFX.fxModes[oldFXSlot] != -1) {
                MixtrackPlatinumFX.updateFXButtonLED(MixtrackPlatinumFX.fxModes[oldFXSlot]);
            }
            MixtrackPlatinumFX.updateFXButtonLED(MixtrackPlatinumFX.activeFXSlot);
            if (MixtrackPlatinumFX.fxModes[MixtrackPlatinumFX.activeFXSlot] != -1) {
                MixtrackPlatinumFX.updateFXButtonLED(MixtrackPlatinumFX.fxModes[MixtrackPlatinumFX.activeFXSlot]);
            }
        } else {
            var effectGroup = MixtrackPlatinumFX.getActiveEffectGroup();
            engine.setValue(effectGroup, 'clear', 1);
            engine.setValue(effectGroup, 'enabled', 0);
            var oldMode = MixtrackPlatinumFX.fxModes[MixtrackPlatinumFX.activeFXSlot];
            var newMode = (oldMode == control) ? -1 : control;
            MixtrackPlatinumFX.fxModes[MixtrackPlatinumFX.activeFXSlot] = newMode;
            if (oldMode != -1) {
                MixtrackPlatinumFX.updateFXButtonLED(oldMode);
            }
            if (newMode != -1) {
                MixtrackPlatinumFX.updateFXButtonLED(newMode);
                for (var i = 0; i < MixtrackPlatinumFX.fxModeMap[newMode]; ++i) {
                    engine.setValue(effectGroup, 'next_effect', 1);
                }
                engine.setValue(effectGroup, 'enabled', 1);
            }
        }
    }
};

MixtrackPlatinumFX.fxPaddle = function(channel, control, value, status, group) {
    var deck = MixtrackPlatinumFX.activeDecks[channel-0x08];
    engine.setValue(MixtrackPlatinumFX.getActiveEffectUnitGroup(), 'group_[Channel'+(deck+1)+']_enable', value==0x00 ? 0 : 1);
};

MixtrackPlatinumFX.updateFXButtonLED = function(button) {
    var status = 0x90 + (button < 3 ? 8 : 9);
    var value = 0x00;
    if (MixtrackPlatinumFX.fxModes[MixtrackPlatinumFX.activeFXSlot] == button) {
        value = 0x7f;
    } else if (MixtrackPlatinumFX.activeFXSlot == button && MixtrackPlatinumFX.blinkState) {
        value = 0x03;
    } else if (MixtrackPlatinumFX.fxModes[button] != -1) {
        value = 0x01;
    }
    midi.sendShortMsg(status, button, value);
}

MixtrackPlatinumFX.init = function(id, debug) {

    // bpm guide up
    //midi.sendShortMsg(0x90, 0x09, 0x00);
    // bpm guide down
    //midi.sendShortMsg(0x90, 0x0a, 0x00);
    
    MixtrackPlatinumFX.deckGroups = ["[Channel1]", "[Channel2]", "[Channel3]", "[Channel4]"];
    MixtrackPlatinumFX.groupChannels = {};
    MixtrackPlatinumFX.shifted = false;
    MixtrackPlatinumFX.activeDecks = [0, 1];
    MixtrackPlatinumFX.deckRateRangeIdxs = [0, 0, 0, 0];
    MixtrackPlatinumFX.deckScratchModes = [false, false, false, false];
    MixtrackPlatinumFX.deckScratchStates = [0, 0, 0, 0];
    MixtrackPlatinumFX.deckLastScratchTicks = [0, 0, 0, 0];
    MixtrackPlatinumFX.deckFastSeekAccums = [0, 0, 0, 0];
    MixtrackPlatinumFX.deckWheelStopTimers = [0, 0, 0, 0];
    MixtrackPlatinumFX.deckPadModes = [0, 0, 0, 0];
    MixtrackPlatinumFX.deckPadLEDConnections = [[],[],[],[]];
    MixtrackPlatinumFX.activeFXSlot = 0;
    MixtrackPlatinumFX.fxModes = [-1, -1, -1, -1, -1, -1];

    for (var deck = 0; deck < 4; ++deck) {
        var deckGroup = MixtrackPlatinumFX.deckGroups[deck];
        MixtrackPlatinumFX.groupChannels[deckGroup] = deck;
        for (var mode = 0; mode < 6; ++mode) {
            MixtrackPlatinumFX.deckPadLEDConnections[deck].push([]);
        }
    }

    MixtrackPlatinumFX.instantDoubles = false;
    MixtrackPlatinumFX.instantDoublesTimeoutID = 0;

    MixtrackPlatinumFX.blinkState = 0;

    // exit demo mode
    sendSysex([0xf0, 0x00, 0x20, 0x7f, 0x03, 0x01, 0xf7]);
    sendSysex([0xf0, 0x7e, 0x00, 0x06, 0x01, 0xf7]);
    //sendSysex([0xf0, 0x00, 0x20, 0x7f, 0x13, 0xf7]);

    // Make sure decks 1 and 2 are active initially
    midi.sendShortMsg(0x90, 0x08, 0x7f);
    midi.sendShortMsg(0x91, 0x08, 0x7f);
    
    for (var deck = 0; deck < 4; ++deck) {
        var deckGroup = MixtrackPlatinumFX.deckGroups[deck];
        engine.makeConnection(deckGroup, 'bpm', MixtrackPlatinumFX.bpmCallback).trigger();
        engine.makeConnection(deckGroup, 'playposition', MixtrackPlatinumFX.positionCallback);
        engine.makeConnection(deckGroup, 'rate', MixtrackPlatinumFX.rateCallback).trigger();

        engine.makeConnection(deckGroup, 'VuMeter', MixtrackPlatinumFX.vuCallback).trigger();

        var initPos = engine.getValue(deckGroup, "track_loaded") ? engine.getValue(deckGroup, "playposition") : 0;
        MixtrackPlatinumFX.positionCallback(initPos, deckGroup, "playposition");

        for (var control in MixtrackPlatinumFX.genericLEDs) {
            engine.makeConnection(deckGroup, control, MixtrackPlatinumFX.genericLEDCallback).trigger();
        }
        for (var control in MixtrackPlatinumFX.binaryLEDs) {
            engine.makeConnection(deckGroup, control, MixtrackPlatinumFX.binaryLEDCallback).trigger();
        }

        for (var padMode = 0; padMode < 6; ++padMode) {
            MixtrackPlatinumFX.updatePadModeLED(deck, padMode);
        }

        MixtrackPlatinumFX.makePadHotcueLEDConnections(deck);
        MixtrackPlatinumFX.makePadAutoloopLEDConnections(deck);
        MixtrackPlatinumFX.makePadBeatjumpLEDConnections(deck);
        MixtrackPlatinumFX.makePadSamplerLEDConnections(deck);

        MixtrackPlatinumFX.updatePadLEDs(deck);

        MixtrackPlatinumFX.updateRateRange(deck, MixtrackPlatinumFX.rateRangeOptions[0]);
        MixtrackPlatinumFX.toggleScratch(deck);
    }

    engine.beginTimer(500, MixtrackPlatinumFX.blinkCallback, false);

    for (var fxButton = 0; fxButton < 6; ++fxButton) {
        MixtrackPlatinumFX.updateFXButtonLED(fxButton);
    }
    
};

MixtrackPlatinumFX.shutdown = function() {
};