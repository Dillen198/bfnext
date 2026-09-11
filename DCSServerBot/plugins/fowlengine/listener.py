from core import EventListener, Server, event
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .commands import FowlEngine

class FowlEngineEventListener(EventListener["FowlEngine"]):
    """
    Listens to Fowl Engine events from the DCS server.
    """

    @event(name="registerDCSServer")
    async def registerDCSServer(self, server: Server, _data: dict) -> None:
        self.log.debug(f"FowlEngine plugin loaded for server {server.name}")

    @event(name="vs_event")
    async def vs_event(self, server: Server, data: dict) -> None:
        """
        Receives async events from Fowl Engine.
        Data should contain:
        - type: 'alert' or 'achievement'
        - message: The message text
        """
        event_type = data.get('type', 'alert')
        message = data.get('message', '')
        
        self.log.info(f"FowlEngine {event_type}: {message}")
        
        config = self.plugin.get_config(server)
        if not config:
            return
            
        channel_id = None
        if event_type == 'alert' and 'alerts_channel' in config:
            channel_id = int(config['alerts_channel'])
        elif event_type == 'achievement' and 'achievements_channel' in config:
            channel_id = int(config['achievements_channel'])
        elif event_type in ['capture', 'neutral', 'ready_to_capture'] and 'alerts_channel' in config:
            channel_id = int(config['alerts_channel'])
            
        if channel_id:
            channel = self.plugin.bot.get_channel(channel_id)
            if channel:
                messages = config.get('messages', {})
                if event_type == 'achievement':
                    fmt = messages.get('achievement', "🎖️ **[ACHIEVEMENT]** {message}")
                elif event_type == 'capture':
                    fmt = messages.get('capture', "🏆 **[CAPTURED]** {message}")
                elif event_type == 'neutral':
                    fmt = messages.get('neutral', "🏳️ **[NEUTRAL]** {message}")
                elif event_type == 'ready_to_capture':
                    fmt = messages.get('ready_to_capture', "⏳ **[READY TO CAPTURE]** {message}")
                else:
                    fmt = messages.get('alert', "🚨 **[ALERT]** {message}")
                    
                await channel.send(fmt.format(message=message))
