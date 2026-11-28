"""Optional rapid cool-down at the end of a Bambu print (2026-11-28).

After the hotends are off and the head is parked, run the auxiliary fans and the exhaust
fan at full speed for N minutes, then switch them off, so the part and plate cool quickly
for removal. The block goes into the machine end G-code of a copy of the machine preset,
just before the stock "finish air purification" section; nothing else changes.

Fan numbers (M106 P#) come from the stock end G-code of each machine:
  X2D:          P2 remote part cooling (aux), P10 remote part1 cooling (aux), P3 chamber exhaust
  X1C/P1S:      P2 auxiliary, P3 chamber
The printer reports the job as still running while it waits, so the next job cannot start
until the cool-down ends.

  from bambu_cooldown import with_cooldown
  machine = with_cooldown(json.loads(machine_json.read_text()), minutes=10)
"""

FANS = {'X2D': ('P2', 'P10', 'P3'), 'X1C': ('P2', 'P3'), 'P1S': ('P2', 'P3')}
ANCHOR = ';=====printer finish air purification========='

def fans_for(machine):
    name = machine.get('name', '') + ' ' + machine.get('printer_model', '')
    for key, fans in FANS.items():
        if key in name: return fans
    raise ValueError(f'no cool-down fan map for machine {name.strip()!r}')

def block(fans, minutes):
    on = ''.join(f'M106 {p} S255\n' for p in fans)
    off = ''.join(f'M106 {p} S0\n' for p in fans)
    return (f';===== coscad rapid cool-down: {" ".join(fans)} at full speed for {minutes:g} min =====\n'
            f'{on}M400 S{round(minutes*60)}\n{off};===== coscad rapid cool-down end =====\n\n')

def with_cooldown(machine, minutes):
    """A copy of the machine preset whose end G-code runs the cool-down fans for `minutes`."""
    if minutes <= 0: return machine
    end = machine['machine_end_gcode']
    end = end if isinstance(end, str) else end[0]
    assert end.count(ANCHOR) >= 1, 'end G-code has no finish air purification section to anchor on'
    i = end.index(ANCHOR)
    out = dict(machine)
    out['machine_end_gcode'] = end[:i] + block(fans_for(machine), minutes) + end[i:]
    return out
