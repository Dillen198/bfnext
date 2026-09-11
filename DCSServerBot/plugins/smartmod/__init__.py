# plugins/smartmod/__init__.py
from .commands import Smartmod

__version__ = "1.0.0"

async def setup(bot):
    await bot.add_cog(Smartmod(bot))