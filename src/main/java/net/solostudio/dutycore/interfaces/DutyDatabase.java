package net.solostudio.dutycore.interfaces;

import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface DutyDatabase {
    boolean isConnected();

    void disconnect();

    void reconnect();

    void createTable();

    void joinDuty(@NotNull String playerName);

    void leaveDuty(@NotNull String playerName);

    void createPlayer(@NotNull Player player);

    boolean exists(@NotNull String playerName);

    String getFormattedDutyTime(@NotNull String playerName);

    String getUUID(@NotNull String playerName);

    String getFormattedServedTime(@NotNull String playerName);

    void updateDutyTime(@NotNull String playerName);

    void updateServedTime(@NotNull String playerName);

    void clearServedTime(@NotNull String playerName);

    boolean isInDuty(@NotNull String playerName);

    List<String> getEveryStaffInDatabase();

    void deletePlayer(@NotNull String playerName);
}
