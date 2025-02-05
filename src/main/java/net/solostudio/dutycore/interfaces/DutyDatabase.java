package net.solostudio.dutycore.interfaces;

import net.solostudio.dutycore.data.StaffData;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface DutyDatabase {
    boolean isConnected();

    void disconnect();

    void reconnect();

    void createTable();

    void setPlayerRank(@NotNull String playerName, @NotNull String playerRank);

    void setStaffRank(@NotNull String playerName, @NotNull String staffRank);

    void joinDuty(@NotNull String playerName);

    void leaveDuty(@NotNull String playerName);

    void createPlayer(@NotNull Player player);

    void setBadge(@NotNull String playerName, @NotNull String badgeKey);

    String getBadge(@NotNull String playerName);

    boolean exists(@NotNull String playerName);

    String getFormattedDutyTime(@NotNull String playerName);

    String getUUID(@NotNull String playerName);

    String getPlayerRank(@NotNull String playerName);

    String getStaffRank(@NotNull String playerName);

    String getFormattedServedTime(@NotNull String playerName);

    void updateDutyTime(@NotNull String playerName);

    void updateServedTime(@NotNull String playerName);

    void clearServedTime(@NotNull String playerName);

    boolean isInDuty(@NotNull String playerName);

    List<String> getEveryStaffInDatabase();

    CompletableFuture<List<StaffData>> getStaffDatas();

    void deletePlayer(@NotNull String playerName);

    void resetDutyTime(@NotNull String playerName);
}
