__version__ = "1.0.0"

from core import Plugin
from .commands import VectorStrike
from .listener import VectorStrikeEventListener

async def setup(bot):
    await bot.add_cog(VectorStrike(bot, VectorStrikeEventListener))
