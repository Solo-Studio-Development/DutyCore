package net.solostudio.dutycore.commands;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.annotations.Badges;
import net.solostudio.dutycore.annotations.Staffs;
import net.solostudio.dutycore.data.MenuController;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.enums.keys.MessageKeys;
import net.solostudio.dutycore.events.AdminChatEvent;
import net.solostudio.dutycore.events.DutyJoinEvent;
import net.solostudio.dutycore.events.DutyLeaveEvent;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.menu.menus.NavigationMenu;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import revxrsal.commands.annotation.Subcommand;
import revxrsal.commands.bukkit.annotation.CommandPermission;
import revxrsal.commands.orphan.OrphanCommand;

import java.util.ArrayList;
import java.util.List;

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
        DutyDatabase database = DutyCore.getDatabase();

        if (database.isInDuty(name)) {
            player.sendMessage(MessageKeys.ALREADY_DUTY.getMessage());
            return;
        }

        DutyCore.getInstance().getServer().getPluginManager().callEvent(new DutyJoinEvent(name));
        database.joinDuty(name);
        player.sendMessage(MessageKeys.DUTY_JOIN.getMessage());
    }

    @Subcommand("leave")
    @CommandPermission("dutycore.leave")
    public void leave(@NotNull Player player) {
        String name = player.getName();
        DutyDatabase database = DutyCore.getDatabase();
        String served = database.getFormattedServedTime(name);
        String time = database.getFormattedDutyTime(name);

        if (!database.isInDuty(name)) {
            player.sendMessage(MessageKeys.REQUIRES_DUTY.getMessage());
            return;
        }

        player.sendMessage(MessageKeys.DUTY_LEAVE.getMessage()
                .replace("{served}", served)
                .replace("{time}", time));
        DutyCore.getInstance().getServer().getPluginManager().callEvent(new DutyLeaveEvent(name, served, time));
        database.leaveDuty(name);
    }

    @Subcommand("forceleave")
    @CommandPermission("dutycore.forceleave")
    public void forceLeave(@NotNull CommandSender sender, @NotNull @Staffs String target) {
        DutyDatabase database = DutyCore.getDatabase();
        String served = database.getFormattedServedTime(target);
        String time = database.getFormattedDutyTime(target);

        if (target.isBlank()) {
            sender.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        if (!database.isInDuty(target)) {
            sender.sendMessage(MessageKeys.REQUIRES_DUTY_OTHER.getMessage());
            return;
        }

        DutyCore.getInstance().getServer().getPluginManager().callEvent(new DutyLeaveEvent(target, served, time));
        database.leaveDuty(target);
        sender.sendMessage(MessageKeys.SUCCESS_FORCE_LEAVE.getMessage());
    }

    @Subcommand("forcejoin")
    @CommandPermission("dutycore.forcejoin")
    public void forceJoin(@NotNull CommandSender sender, @NotNull @Staffs String target) {
        DutyDatabase database = DutyCore.getDatabase();

        if (target.isBlank()) {
            sender.sendMessage(MessageKeys.PLAYER_REQUIRED.getMessage());
            return;
        }

        if (database.isInDuty(target)) {
            sender.sendMessage(MessageKeys.ALREADY_DUTY_OTHER.getMessage());
            return;
        }

        DutyCore.getInstance().getServer().getPluginManager().callEvent(new DutyJoinEvent(target));
        database.joinDuty(target);
        sender.sendMessage(MessageKeys.SUCCESS_FORCE_JOIN.getMessage());
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
        if (!DutyCore.getDatabase().isInDuty(player.getName())) {
            player.sendMessage(MessageKeys.REQUIRES_DUTY.getMessage());
            return;
        }

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

        MessageProcessor.translateInformation(target, MessageKeys.STATS.getMessages()).thenAccept(message -> message.forEach(player::sendMessage));
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

    @Subcommand("menu")
    @CommandPermission("dutycore.menu")
    public void menu(@NotNull Player player) {
        new NavigationMenu(MenuController.getMenuUtils(player)).open();
    }
}
