# Joining a Team

Before you can fly or fight, you must be registered to either the Blue or Red coalition.

## Registering

With dynamic spawn you **no longer type a coalition in chat**. Just pick a coalition
and a slot from the normal DCS role/slot selection screen:

1. Connect to the server and open the slot selection screen
2. Choose **Blue** or **Red**
3. Select any available aircraft or ground unit slot for that coalition

The first time you take a slot, the engine registers you to that coalition
automatically and confirms in chat:

> "Welcome to the Blue team. You may only occupy slots belonging to your team. Good luck!"

### Important Notes

- Registration happens the moment you take your first slot &mdash; there is nothing to type
- Once registered you can only occupy slots for your coalition
- Registration is **permanent** by default (see Side Switching below)
- The server announces your coalition choice to all players
- Picking a slot on the wrong coalition after you are registered is rejected; go back to spectators and use `-switch`

## Side Switching

Switching coalitions is still done from chat, because DCS has no in-game control for it.
The server allows a limited number of side switches (often **1**).

```
-switch blue
```
or
```
-switch red
```

**Requirements**:

- You must be in **spectator mode** (leave your slot first)
- You must have side switches remaining (check with `-lives`)
- Once your switches are used up, you are locked to that coalition permanently

## What Happens After Registration?

Once registered:

1. **Slot Selection**: You can occupy any slot for your coalition
2. **Team Chat**: Your messages are visible to your coalition
3. **Points Balance**: You start with an initial points balance
4. **Lives**: You receive a starting number of lives
5. **F10 Menus**: Team-specific menus become available

## Checking Your Status

To verify your registration and see your stats:

```
-lives
```

This displays:

- Your current coalition
- Remaining lives
- Points balance
- Side switches remaining (if any)

## Common Issues

### "You are already on the Blue team"
You're already registered! No action needed.

### "You must be in spectators to switch sides"
Leave your current slot and return to spectators before switching coalitions.

### "You are already on {team} team, and you may not switch sides"
You've used all your side switches, or the server doesn't allow switching.

### A wrong-coalition slot is rejected
You picked a slot belonging to the coalition you are not registered to. Either pick
a slot for your coalition, or return to spectators and use `-switch`.

## Next Steps

See [Understanding the Menus](./hud-and-menus.md) for interface documentation.
