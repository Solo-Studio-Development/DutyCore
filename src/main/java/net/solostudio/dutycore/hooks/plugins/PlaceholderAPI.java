package net.solostudio.dutycore.hooks.plugins;

import me.clip.placeholderapi.expansion.PlaceholderExpansion;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.utils.StartingUtils;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("deprecation")
public class PlaceholderAPI {
    public static boolean isRegistered = false;

    public static void registerHook() {
        if (Bukkit.getPluginManager().getPlugin("PlaceholderAPI") != null) {
            new PlaceholderIntegration().register();
            isRegistered = true;
        }
    }

    private static class PlaceholderIntegration extends PlaceholderExpansion {
        @Override
        public @NotNull String getIdentifier() {
            return "dc";
        }

        @Override
        public @NotNull String getAuthor() {
            return "User-19fff";
        }

        @Override
        public @NotNull String getVersion() {
            return DutyCore.getInstance().getDescription().getVersion();
        }

        @Override
        public boolean canRegister() {
            return true;
        }

        @Override
        public boolean persist() {
            return true;
        }

        @Override
        public String onPlaceholderRequest(@NotNull Player player, @NotNull String params) {
            DutyDatabase database = DutyCore.getDatabase();
            String playerName = player.getName();

            return switch (params) {
                case "time" -> database.getFormattedDutyTime(playerName);
                case "served_time" -> database.isInDuty(playerName) ? database.getFormattedServedTime(playerName) : "---";
                case "staff_rank" -> database.getStaffRank(playerName);
                case "player_rank" -> database.getPlayerRank(playerName);
                case "badge" -> database.isInDuty(playerName)
                        ? StartingUtils.getBadges().getOrDefault(database.getBadge(playerName), "")
                        : "";
                case "is_in_duty" -> database.isInDuty(playerName) ? ConfigKeys.TRUE.getString() : ConfigKeys.FALSE.getString();
                default -> "";
            };
        }
    }
}
