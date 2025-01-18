package net.solostudio.dutycore.commands;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.annotations.Badges;
import net.solostudio.dutycore.annotations.Staffs;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.enums.keys.MessageKeys;
import net.solostudio.dutycore.events.AdminChatEvent;
import net.solostudio.dutycore.events.DutyJoinEvent;
import net.solostudio.dutycore.events.DutyLeaveEvent;
import net.solostudio.dutycore.hooks.plugins.LiteBans;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.utils.DutyUtils;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import revxrsal.commands.annotation.Subcommand;
import revxrsal.commands.bukkit.annotation.CommandPermission;
import revxrsal.commands.orphan.OrphanCommand;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public class CommandDuty implements OrphanCommand {
    @Getter public static List<Player> freezeList = new ArrayList<>();

    @Subcommand("reload")
    @CommandPermission("dutycore.reload")
    public void reload(@NotNull CommandSender sender) {
        DutyCore.getInstance().getLanguage().reload();
        DutyCore.getInstance().getConfiguration().reload();
        DutyCore.getInstance().getWebhookFile().reload();
        sender.sendMessage(MessageKeys.RELOAD.getMessage());
    }

    @Subcommand("resetstats")
    @CommandPermission("dutycore.resetstats")
    public void resetStats(@NotNull CommandSender sender, @NotNull @Staffs String target) {
        if (target.isBlank()) {
            sender.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        DutyCore.getDatabase().resetDutyTime(target);
        sender.sendMessage(MessageKeys.STATS_RESET.getMessage());
    }

    @Subcommand("join")
    @CommandPermission("dutycore.join")
    public void join(@NotNull Player player) {
        String name = player.getName();

        DutyCore.getInstance().getServer().getPluginManager().callEvent(new DutyJoinEvent(name));
        DutyCore.getDatabase().joinDuty(name);
        player.sendMessage(MessageKeys.DUTY_JOIN.getMessage());
    }

    @Subcommand("leave")
    @CommandPermission("dutycore.leave")
    public void leave(@NotNull Player player) {
        DutyDatabase database = DutyCore.getDatabase();
        String served = database.getFormattedServedTime(player.getName());
        String time = database.getFormattedDutyTime(player.getName());

        player.sendMessage(MessageKeys.DUTY_LEAVE.getMessage()
                .replace("{served}", served)
                .replace("{time}", time));
        DutyCore.getInstance().getServer().getPluginManager().callEvent(new DutyLeaveEvent(player.getName(), served, time));
        database.leaveDuty(player.getName());
    }

    @Subcommand("chat")
    @CommandPermission("dutycore.chat")
    public void chat(@NotNull Player sender, @NotNull String message) {
        if (!DutyCore.getDatabase().isInDuty(sender.getName())) {
            sender.sendMessage(MessageKeys.REQUIRES_DUTY.getMessage());
            return;
        }

        Bukkit.getOnlinePlayers().stream()
                .filter(player -> player.hasPermission("dutycore.staff"))
                .filter(player -> DutyCore.getDatabase().isInDuty(player.getName()))
                .forEach(player -> player.sendMessage(ConfigKeys.DUTY_CHAT_FORMAT.getString()
                        .replace("{player}", sender.getName())
                        .replace("{message}", (message + " ").trim())));

        DutyCore.getInstance().getServer().getPluginManager().callEvent(new AdminChatEvent(sender.getName(), message));
    }

    @Subcommand("freeze")
    @CommandPermission("dutycore.freeze")
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
    @CommandPermission("dutycore.stats")
    public void stats(@NotNull Player player, @NotNull @Staffs String target) {
        if (target.isBlank()) {
            player.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        DutyDatabase database = DutyCore.getDatabase();

        UUID targetUUID = UUID.fromString(database.getUUID(target));
        CompletableFuture<Integer> bansFuture = LiteBans.getCount("litebans_bans", "banned_by_uuid", targetUUID);
        CompletableFuture<Integer> kicksFuture = LiteBans.getCount("litebans_kicks", "banned_by_uuid", targetUUID);
        CompletableFuture<Integer> mutesFuture = LiteBans.getCount("litebans_mutes", "banned_by_uuid", targetUUID);
        CompletableFuture<Void> combinedFuture = CompletableFuture.allOf(bansFuture, kicksFuture, mutesFuture);

        combinedFuture.thenRun(() -> {
            String bans = String.valueOf(bansFuture.join());
            String kicks = String.valueOf(kicksFuture.join());
            String mutes = String.valueOf(mutesFuture.join());
            String time = database.getFormattedDutyTime(target);

            MessageKeys.STATS.getMessages().forEach(message -> {
                player.sendMessage(message
                        .replace("{bans}", bans)
                        .replace("{kicks}", kicks)
                        .replace("{mutes}", mutes)
                        .replace("{dTime}", time)
                        .replace("{name}", target)
                        .replace("{inDuty}", database.isInDuty(target) ? ConfigKeys.TRUE.getString() : ConfigKeys.FALSE.getString())
                        .replace("{sRank}", database.getStaffRank(target))
                        .replace("{pRank}", database.getPlayerRank(target))
                        .replace("{badge}", database.getBadge(target)));
            });
        });
    }

    @Subcommand("set staff")
    @CommandPermission("dutycore.set.staff")
    public void setStaff(@NotNull CommandSender sender, @NotNull @Staffs String target, @NotNull String rank) {
        if (target.isBlank()) {
            sender.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        DutyCore.getDatabase().setStaffRank(target, rank);
        sender.sendMessage(MessageKeys.STAFF_RANK.getMessage());
    }

    @Subcommand("set player")
    @CommandPermission("dutycore.set.player")
    public void setPlayer(@NotNull CommandSender sender, @NotNull @Staffs String target, @NotNull String rank) {
        if (target.isBlank()) {
            sender.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        DutyCore.getDatabase().setPlayerRank(target, rank);
        sender.sendMessage(MessageKeys.PLAYER_RANK.getMessage());
    }

    @Subcommand("set badge")
    @CommandPermission("dutycore.set.badge")
    public void setBadge(@NotNull CommandSender sender, @NotNull @Staffs String target, @NotNull @Badges String badge) {
        if (target.isBlank()) {
            sender.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        DutyCore.getDatabase().setBadge(target, badge);
        sender.sendMessage(MessageKeys.SUCCESS_BADGE.getMessage());
    }
}
