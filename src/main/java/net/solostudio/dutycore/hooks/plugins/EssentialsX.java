package net.solostudio.dutycore.hooks.plugins;

import net.solostudio.dutycore.DutyCore;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

public class EssentialsX {
    public static boolean isAFK(@NotNull Player player) {
        return DutyCore.getInstance().getEssentials().getUser(player).isAfk();
    }
}
