# plugins/fowlengine/__init__.py
from .commands import FowlEngine

__version__ = "1.0.0"

async def setup(bot):
    await bot.add_cog(FowlEngine(bot))
