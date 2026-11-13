# plugins/radio/__init__.py
from .commands import Radio

__version__ = "1.0.0"

async def setup(bot):
    await bot.add_cog(Radio(bot))