package net.solostudio.dutycore.processor;

import lombok.experimental.UtilityClass;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.enums.keys.MessageKeys;
import net.solostudio.dutycore.hooks.plugins.LiteBans;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import org.bukkit.ChatColor;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@UtilityClass
public class MessageProcessor {
    private static final Pattern HEX_PATTERN = Pattern.compile("#[a-fA-F\\d]{6}");

    public @NotNull String process(@Nullable String message) {
        if (message == null) return "";

        Matcher matcher = HEX_PATTERN.matcher(message);
        StringBuilder result = new StringBuilder();

        while (matcher.find()) {
            String hexCode = matcher.group();
            StringBuilder builder = new StringBuilder();

            for (char c : hexCode.substring(1).toCharArray()) {
                builder.append("&").append(c);
            }

            matcher.appendReplacement(result, builder.toString());
        }

        matcher.appendTail(result);

        return ChatColor.translateAlternateColorCodes('&', result.toString());
    }

    public CompletableFuture<List<String>> translateInformation(@NotNull String player, List<String> message) {
        DutyDatabase database = DutyCore.getDatabase();
        UUID targetUUID = UUID.fromString(database.getUUID(player));

        CompletableFuture<Integer> bansFuture = LiteBans.getCount("litebans_bans", "banned_by_uuid", targetUUID);
        CompletableFuture<Integer> kicksFuture = LiteBans.getCount("litebans_kicks", "banned_by_uuid", targetUUID);
        CompletableFuture<Integer> mutesFuture = LiteBans.getCount("litebans_mutes", "banned_by_uuid", targetUUID);
        CompletableFuture<Void> combinedFuture = CompletableFuture.allOf(bansFuture, kicksFuture, mutesFuture);

        List<String> messagesToPlayer = new ArrayList<>();

        return combinedFuture.thenApply(v0id -> {
            String bans = String.valueOf(bansFuture.join());
            String kicks = String.valueOf(kicksFuture.join());
            String mutes = String.valueOf(mutesFuture.join());
            String time = database.getFormattedDutyTime(player);

            message.forEach(msgTemplate -> {
                String processedMessage = msgTemplate
                        .replace("{bans}", bans)
                        .replace("{kicks}", kicks)
                        .replace("{mutes}", mutes)
                        .replace("{dTime}", time)
                        .replace("{name}", player)
                        .replace("{inDuty}", database.isInDuty(player) ? ConfigKeys.TRUE.getString() : ConfigKeys.FALSE.getString())
                        .replace("{sRank}", database.getStaffRank(player))
                        .replace("{pRank}", database.getPlayerRank(player))
                        .replace("{badge}", database.getBadge(player));
                messagesToPlayer.add(processedMessage);
            });

            return messagesToPlayer;
        });
    }
}
