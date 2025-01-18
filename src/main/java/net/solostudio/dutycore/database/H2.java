package net.solostudio.dutycore.database;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.events.DutyJoinEvent;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.utils.DutyUtils;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Getter
public class H2 implements DutyDatabase {
    private final Connection connection;
    private final HikariDataSource dataSource;

    public H2() throws SQLException, ClassNotFoundException {
        Class.forName("net.solostudio.dutycore.libs.h2.Driver");

        HikariConfig hikariConfig = new HikariConfig();

        String url = "jdbc:h2:" + DutyCore.getInstance().getDataFolder().getAbsolutePath() + "/database;MODE=MySQL";

        hikariConfig.setJdbcUrl(url);
        hikariConfig.setUsername("sa");
        hikariConfig.setPassword("");
        hikariConfig.setMaximumPoolSize(10);
        hikariConfig.setPoolName("DutyCorePool");

        dataSource = new HikariDataSource(hikariConfig);
        connection = dataSource.getConnection();
    }

    @Override
    public boolean isConnected() {
        return getConnection() != null;
    }

    @Override
    public void disconnect() {
        if (isConnected()) {
            try {
                getConnection().close();
            } catch (SQLException exception) {
                LoggerUtils.error(exception.getMessage());
            }
        }
    }

    @Override
    public void reconnect() {
        try {
            if (getConnection() != null && !getConnection().isClosed()) getConnection().close();
            new H2();
        } catch (SQLException | ClassNotFoundException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void createTable() {
        String dutyTableQuery = "CREATE TABLE IF NOT EXISTS duty (PLAYER VARCHAR(255) NOT NULL PRIMARY KEY, IN_DUTY TINYINT(1) DEFAULT 0, DUTY_TIME BIGINT DEFAULT 0, SERVED_TIME BIGINT DEFAULT 0, UUID VARCHAR(255), PLAYER_RANK VARCHAR(255), STAFF_RANK VARCHAR(255), BADGE VARCHAR(255))";

        try (PreparedStatement dutyTableStatement = getConnection().prepareStatement(dutyTableQuery)) {
            dutyTableStatement.execute();
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void setBadge(@NotNull String playerName, @NotNull String badgeKey) {
        String query = "UPDATE duty SET BADGE = ? WHERE PLAYER = ?";

        try (PreparedStatement statement = connection.prepareStatement(query)) {
            statement.setString(1, badgeKey);
            statement.setString(2, playerName);
            statement.executeUpdate();
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public String getBadge(@NotNull String playerName) {
        String query = "SELECT BADGE FROM duty WHERE PLAYER = ?";

        try (PreparedStatement statement = connection.prepareStatement(query)) {
            statement.setString(1, playerName);
            ResultSet resultSet = statement.executeQuery();
            if (resultSet.next()) return resultSet.getString("BADGE");
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
        return null;
    }

    @Override
    public void createPlayer(@NotNull Player player) {
        String query = "INSERT IGNORE INTO duty (PLAYER, UUID) VALUES (?, ?)";

        try {
            if (!exists(player.getName())) {
                try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                    preparedStatement.setString(1, player.getName());
                    preparedStatement.setString(2, player.getUniqueId().toString());
                    preparedStatement.executeUpdate();
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public boolean exists(@NotNull String playerName) {
        String query = "SELECT * FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                return preparedStatement.executeQuery().next();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return false;
    }

    @Override
    public void setPlayerRank(@NotNull String playerName, @NotNull String playerRank) {
        String query = "UPDATE duty SET PLAYER_RANK = ? WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerRank);
                preparedStatement.setString(2, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void setStaffRank(@NotNull String playerName, @NotNull String staffRank) {
        String query = "UPDATE duty SET STAFF_RANK = ? WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, staffRank);
                preparedStatement.setString(2, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void joinDuty(@NotNull String playerName) {
        String query = "UPDATE duty SET IN_DUTY = 1 WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        Bukkit.dispatchCommand(Bukkit.getConsoleSender(), "lp user " + playerName + " parent set " + getStaffRank(playerName));
    }

    @Override
    public void leaveDuty(@NotNull String playerName) {
        String query = "UPDATE duty SET IN_DUTY = 0 WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        Bukkit.dispatchCommand(Bukkit.getConsoleSender(), "lp user " + playerName + " parent set " + getPlayerRank(playerName));
    }

    @Override
    public String getFormattedDutyTime(@NotNull String playerName) {
        String query = "SELECT DUTY_TIME FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        long dutyTime = resultSet.getLong("DUTY_TIME");
                        long hours = dutyTime / 3600;
                        long minutes = (dutyTime % 3600) / 60;
                        long seconds = dutyTime % 60;

                        return (hours > 0 ? hours + " óra " : "") +
                                (minutes > 0 ? minutes + " perc " : "") +
                                seconds + " mp";
                    }
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return "0 mp";
    }

    @Override
    public String getUUID(@NotNull String playerName) {
        String query = "SELECT UUID FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) return resultSet.getString("UUID");
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return "NULL";
    }

    @Override
    public String getPlayerRank(@NotNull String playerName) {
        String query = "SELECT PLAYER_RANK FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) return resultSet.getString("PLAYER_RANK");
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return "NULL";
    }

    @Override
    public String getStaffRank(@NotNull String playerName) {
        String query = "SELECT STAFF_RANK FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) return resultSet.getString("STAFF_RANK");
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return "NULL";
    }

    @Override
    public String getFormattedServedTime(@NotNull String playerName) {
        String query = "SELECT SERVED_TIME FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        long dutyTime = resultSet.getLong("SERVED_TIME");
                        long hours = dutyTime / 3600;
                        long minutes = (dutyTime % 3600) / 60;
                        long seconds = dutyTime % 60;

                        return (hours > 0 ? hours + " óra " : "") +
                                (minutes > 0 ? minutes + " perc " : "") +
                                seconds + " mp";
                    }
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return "0 mp";
    }

    @Override
    public void updateDutyTime(@NotNull String playerName) {
        String query = "UPDATE duty SET DUTY_TIME = DUTY_TIME + 1 WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void updateServedTime(@NotNull String playerName) {
        String query = "UPDATE duty SET SERVED_TIME = SERVED_TIME + 1 WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void clearServedTime(@NotNull String playerName) {
        String query = "UPDATE duty SET SERVED_TIME = 0 WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public boolean isInDuty(@NotNull String playerName) {
        String query = "SELECT IN_DUTY FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) return resultSet.getBoolean("IN_DUTY");
                }
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return false;
    }

    @Override
    public List<String> getEveryStaffInDatabase() {
        List<String> staffs = new ArrayList<>();
        String query = "SELECT PLAYER FROM duty";

        try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
            ResultSet resultSet = preparedStatement.executeQuery();

            while (resultSet.next()) staffs.add(resultSet.getString("PLAYER"));
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        return staffs;
    }

    @Override
    public void deletePlayer(@NotNull String playerName) {
        String query = "DELETE FROM duty WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }

    @Override
    public void resetDutyTime(@NotNull String playerName) {
        String query = "UPDATE duty SET DUTY_TIME = 0 WHERE PLAYER = ?";

        try {
            try (PreparedStatement preparedStatement = getConnection().prepareStatement(query)) {
                preparedStatement.setString(1, playerName);
                preparedStatement.executeUpdate();
            }
        } catch (SQLException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }
}
