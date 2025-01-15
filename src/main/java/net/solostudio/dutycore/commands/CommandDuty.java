package net.solostudio.dutycore.commands;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.annotations.Staffs;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.enums.keys.MessageKeys;
import net.solostudio.dutycore.hooks.plugins.LiteBans;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import revxrsal.commands.annotation.Subcommand;
import revxrsal.commands.bukkit.annotation.CommandPermission;
import revxrsal.commands.orphan.OrphanCommand;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public class CommandDuty implements OrphanCommand {
    @Getter public static List<Player> freezeList = new ArrayList<>();

    @Subcommand("join")
    @CommandPermission("dutycore.staff")
    public void join(@NotNull Player player) {
        DutyCore.getDatabase().joinDuty(player.getName());
        player.sendMessage(MessageKeys.DUTY_JOIN.getMessage());
    }

    @Subcommand("leave")
    @CommandPermission("dutycore.staff")
    public void leave(@NotNull Player player) {
        player.sendMessage(MessageKeys.DUTY_LEAVE.getMessage()
                .replace("{served}", DutyCore.getDatabase().getFormattedServedTime(player.getName()))
                .replace("{time}", DutyCore.getDatabase().getFormattedDutyTime(player.getName())));
        DutyCore.getDatabase().leaveDuty(player.getName());
    }

    @Subcommand("chat")
    @CommandPermission("dutycore.staff")
    public void chat(@NotNull Player sender, @NotNull String message) {
        if (!DutyCore.getDatabase().isInDuty(sender.getName())) {
            sender.sendMessage(MessageKeys.REQUIRES_DUTY.getMessage());
            return;
        }

        Bukkit.getOnlinePlayers().stream()
                .filter(player -> player.hasPermission("dutycore.staff"))
                .filter(player -> DutyCore.getDatabase().isInDuty(player.getName()))
                .forEach(player -> player.sendMessage(ConfigKeys.DUTY_CHAT_FORMAT.getString()
                        .replace("{name}", sender.getName())
                        .replace("{message}", (message + " ").trim())));

    }

    @Subcommand("freeze")
    @CommandPermission("dutycore.staff")
    public void freeze(@NotNull Player player, @NotNull Player target) {
        if (getFreezeList().contains(target)) {
            getFreezeList().remove(target);
            player.sendMessage(MessageKeys.FREEZE_OFF_SENDER.getMessage());
            target.sendMessage(MessageKeys.FREEZE_OFF_TARGET.getMessage());
        } else {
            getFreezeList().add(target);
            player.sendMessage(MessageKeys.FREEZE_ON_SENDER.getMessage());
            target.sendMessage(MessageKeys.FREEZE_ON_TARGET.getMessage());
        }
    }

    @Subcommand("stats")
    @CommandPermission("dutycore.staff")
    public void stats(@NotNull Player player, @NotNull @Staffs String target) {
        if (target.isBlank()) {
            player.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        UUID targetUUID = UUID.fromString(DutyCore.getDatabase().getUUID(target));
        CompletableFuture<Integer> bansFuture = LiteBans.getCount("litebans_bans", "banned_by_uuid", targetUUID);
        CompletableFuture<Integer> kicksFuture = LiteBans.getCount("litebans_kicks", "banned_by_uuid", targetUUID);
        CompletableFuture<Integer> mutesFuture = LiteBans.getCount("litebans_mutes", "banned_by_uuid", targetUUID);
        CompletableFuture<Void> combinedFuture = CompletableFuture.allOf(bansFuture, kicksFuture, mutesFuture);

        combinedFuture.thenRun(() -> {
            String bans = String.valueOf(bansFuture.join());
            String kicks = String.valueOf(kicksFuture.join());
            String mutes = String.valueOf(mutesFuture.join());
            String time = DutyCore.getDatabase().getFormattedDutyTime(target);

            MessageKeys.STATS.getMessages().forEach(message -> {
                player.sendMessage(message
                        .replace("{bans}", bans)
                        .replace("{kicks}", kicks)
                        .replace("{mutes}", mutes)
                        .replace("{dTime}", time)
                        .replace("{name}", target)
                        .replace("{inDuty}", DutyCore.getDatabase().isInDuty(target) ? ConfigKeys.TRUE.getString() : ConfigKeys.FALSE.getString()));
            });
        });
    }
}
