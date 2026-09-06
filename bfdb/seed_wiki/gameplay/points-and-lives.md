# Points and Lives System

Fowl Engine uses a points and lives system to reward good play and encourage careful decision-making.

## Points System

### What Are Points?

Points are the campaign currency:
- Earned through successful missions
- Spent on deployments and actions
- Tracked per player
- Persistent across server restarts

### Earning Points

**Combat Actions**:
- Destroying enemy units
- Successful strikes on objectives
- Air-to-air kills
- Ground kills

**Strategic Actions**:
- **Capturing objectives** - 50 points (split among participants)

**Teamwork**:
- Multi-player captures share the 50 points equally
- Example: 3 players capture = ~17 points each

### Point Values

Point values vary by:
- Target type (aircraft > tanks > infantry)
- Strategic importance
- Server configuration
- Objective value

**Actual Point Values** (PG Tempest):
- **Ground kill**: **2 points**
- **Air kill**: **25 points**
- **LR SAM bonus**: **+5 points** (for killing long-range SAMs)
- **Objective capture**: **50 points** (split among participants)
- **New player bonus**: **190 points** (starting balance)

### Spending Points

**Deployable Units**:
```
F10 → Actions → [Deploy Action] → [Location]
```

**Actual Costs** (PG Tempest):
- **Drone** (Small): **50 points**
- **Drone** (Large MQ-9): **100 points**
- **Paratroops**: **100 points**
- **Light Attack Helicopters**: **120 points**
- **Naval FARP Destroyer**: **100 points**
- **Naval FARP Carrier**: **150 points**
- **Fighters** (CAP): **200 points**
- **SEAD Package**: **200 points**
- **Attack Helicopters**: **200-300 points**
- **Bomber**: **200 points**
- **ALCM** (Cruise Missile Platform): **25-150 points**

**Special Actions**:
- **Logistics Repair**: **100 points** (helo), **200 points** (fast)
- **Logistics Transfer**: **100 points**
- **Deploy EWR Radar**: **50 points**
- **AWACS**: **50 points**
- **Waypoint Commands**: **5-15 points**
- **RTB (Return to Base)**: **0 points**

### Checking Your Balance

**Via Chat**:
```
-balance
```

Response:
```
You have 1250 points
```

**Via Lives Command**:
```
-lives
```

Shows complete status including points.

## Lives System

### What Are Lives?

Lives represent your ability to continue flying:
- Limited number per player
- Lost when you die
- Prevents reckless behavior
- Encourages careful planning

### Starting Lives

Lives vary by aircraft type:
- **Standard** (F-15, F/A-18, Su-27, etc.): **3 lives**
- **Attack** (A-10, Ka-50, AH-64, etc.): **4 lives**
- **Intercept** (MiG-21, F-5, Mirage-F1, etc.): **4 lives**
- **Logistics** (Mi-8, UH-1H, CH-47, etc.): **6 lives**
- **Recon** (L-39, TF-51, Yak-52, etc.): **6 lives**

**Important**: Lives are tracked **separately per role** — Standard, Intercept, Attack, Recon, and Logistics (see the [Aircraft Roster](../reference/aircraft-roster.md) for exact counts) — so running dry in a Standard-role fighter doesn't ground you from flying Recon or Logistics. Each role's life pool refills on its own rolling timer (typically every 6 hours) rather than at round start — check `-lives` in-game to see your current count and role.

### Losing Lives

You lose a life when:
- Your aircraft is destroyed (shot down, crash, or accident)
- You eject and can't be rescued
- Controlled flight into terrain, mid-air collision, or running out of fuel

**Important**:
- Deaths in combat count
- Practice crashes count (be careful!)
- Friendly fire counts — blue-on-blue results in an immediate life penalty

**Note**: Life limits are a campaign setting and can be switched off entirely — if lives aren't being taken on death, that's the server running with unlimited lives, not a bug. Check `-status` or `-lives` in-game to see what's active.

## CSAR — Combat Search & Rescue

![Rescue helicopter hoisting a downed pilot](/api/wiki/images/f0af8d1d-8ae3-4a78-a361-27d14e55aa33)

When a pilot ejects, a downed pilot unit spawns at their crash site instead of an immediate life loss. A friendly helicopter crew can locate and extract them, **restoring the lost life**. The rescuing pilot also earns bonus campaign points for the recovery. If the rescued role is already at max lives, the credit rolls down to the next role instead of being wasted.

**CSAR Process**:
1. **Eject**: Pilot ejects — a downed pilot unit spawns at the crash location on the map.
2. **Locate & Smoke**: A helicopter uses the F10 → CSAR menu to request green smoke on the nearest downed pilot.
3. **Pick Up & Deliver**: Helicopter lands nearby, picks up the pilot, and delivers them to a friendly base or FARP to restore the life.

**Eject over friendly ground** and the pilot recovers themselves — land inside any friendly airbase, FARP or carrier zone and the life is restored automatically, no helicopter required.

**The clock**: a downed pilot is on a capture countdown (a server setting, 30 minutes by default) — let it run out with an enemy nearby and the pilot is lost. A friendly rescue helicopter sitting on top of the pilot pauses that countdown, so a pickup that runs a little long still counts. A pilot already loaded into a helicopter is safe regardless of the timer.

### What Happens at 0 Lives?

**Spectator Mode**:
- Cannot occupy slots
- Can watch the battle
- Still earn points (on some servers)
- Can communicate with team

**Life Reset**:
- Admins can reset your lives
- May require request via Discord
- Some servers have automatic reset timers
- Depends on server policy

### Checking Your Lives

**Via Chat**:
```
-lives
```

Response example:
```
Team: Blue
Lives: 5
Points: 1250
Side Switches: 1
```

## Point Transfers

Some servers allow point transfers between players.

**Transfer Command**:
```
-transfer <amount> <player-name>
-transfer <amount> objective:<objective-name>
```

**Examples**:
```
-transfer 500 Viper21
-transfer 100 objective:Batumi
```

**Use Cases**:
- Help new players
- Pool for expensive deployment
- Contribute points to objectives
- Reimburse friendly fire
- Strategic coordination

**Restrictions**:
- Cannot transfer to enemy team
- Must have sufficient points

## Admin Point Management

Admins can manage points:

**Check Balance**:
```
-admin balance <player>
```

**Set Points**:
```
-admin set-points <amount> <player>
```

**Reset Lives**:
```
-admin reset-lives <player>
```

## Next Steps

Now learn about [Chat Commands](./chat-commands.md) to interact with the system!
