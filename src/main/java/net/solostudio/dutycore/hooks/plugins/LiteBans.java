package net.solostudio.dutycore.hooks.plugins;

import litebans.api.Database;
import lombok.experimental.UtilityClass;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.jetbrains.annotations.NotNull;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@UtilityClass
public class LiteBans {
    private static final String COUNT_QUERY = "SELECT COUNT(*) FROM %s WHERE %s = ?";

    public CompletableFuture<Integer> getCount(@NotNull String tableName, @NotNull String columnName, @NotNull UUID uuid) {
        String query = String.format(COUNT_QUERY, tableName, columnName);
        CompletableFuture<Integer> futureCount = new CompletableFuture<>();

        DutyCore.getInstance().getScheduler().runTaskAsynchronously(() -> {
            int count = 0;
            try (PreparedStatement preparedStatement = Database.get().prepareStatement(query)) {
                preparedStatement.setString(1, uuid.toString());

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) count = resultSet.getInt(1);
                }
            } catch (SQLException exception) {
                LoggerUtils.error(exception.getMessage());
                futureCount.completeExceptionally(exception);
                return;
            }

            futureCount.complete(count);
        });

        return futureCount;
    }
}