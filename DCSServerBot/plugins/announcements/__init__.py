from .commands import Announcements

__version__ = "1.0.0"


async def setup(bot):
    await bot.add_cog(Announcements(bot))