#' @title Get a subplan for graph-based clustering.
#' @description Currently supported community-based detection algorithms are:
#'
#' - [igraph::cluster_leiden()]
#' - [igraph::cluster_louvain()]
#' - [igraph::cluster_walktrap()]
#'
#' See [run_graph_based_clustering()] for more details.
#'
#' @param sce_target_name A character scalar: name of target representing a SCE object that will be used for SNN graph construction.
#' @param dimred A character scalar: which `reducedDim()` to use for clustering.
#' @param cluster_graph_leiden_enabled,cluster_graph_louvain_enabled,cluster_graph_walktrap_enabled
#'   A logical scalar: if `FALSE`, disable the selected clustering and set all corresponding targets to `NULL`.
#' @param cluster_graph_louvain_resolutions_,cluster_graph_leiden_resolutions_ A numeric vector: resolutions to calculate for the
#'  selected clustering method.
#' @param cluster_graph_snn_k_,cluster_graph_snn_type_ Passed to [graph_snn_fn()].
#' @param is_integration A logical scalar: if `TRUE`, clustering results will be named as `cluster_int_graph_<method>[_r<r>]`.
#'   If `FALSE`, they will be named as `cluster_graph_<method>[_r<r>]`.
#' @param plots_out_dir A character scalar: path to output directory to save plots (e.g. [clustree::clustree()]).
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_clustering
#' @export
get_clustering_graph_subplan <- function(sce_target_name,
                                         dimred,
                                         cluster_graph_louvain_enabled,
                                         cluster_graph_louvain_resolutions_,
                                         cluster_graph_walktrap_enabled,
                                         cluster_graph_leiden_enabled,
                                         cluster_graph_leiden_resolutions_,
                                         cluster_graph_snn_k_,
                                         cluster_graph_snn_type_,
                                         is_integration,
                                         plots_out_dir) {
  if (any(cluster_graph_louvain_enabled, cluster_graph_walktrap_enabled, cluster_graph_leiden_enabled)) {
    plan_cluster_graph_common <- drake::drake_plan(
      graph_snn = graph_snn_fn(
        !!sym(sce_target_name),
        snn_k = !!cluster_graph_snn_k_,
        snn_type = !!cluster_graph_snn_type_,
        dimred = !!dimred,
        BPPARAM = ignore(BiocParallel::bpparam())
      )
    )
  } else {
    plan_cluster_graph_common <- drake::drake_plan(
      graph_snn = NULL
    )
  }

  if (cluster_graph_louvain_enabled) {
    if (is_integration) {
      clustree_title <- "cluster_int_graph_louvain"
      clustree_out_file <- "cluster_int_graph_louvain_clustree.pdf"
    } else {
      clustree_out_file <- "cluster_graph_louvain_clustree.pdf"
      clustree_title <- "cluster_graph_louvain"
    }

    plan_cluster_graph_louvain <- drake::drake_plan(
      cluster_graph_louvain_resolutions = !!cluster_graph_louvain_resolutions_,
      cluster_graph_louvain_df = target(
        run_graph_based_clustering(graph_snn, is_integration = !!is_integration, algorithm = "louvain", resolution = cluster_graph_louvain_resolutions),

        dynamic = map(cluster_graph_louvain_resolutions)
      ),
      cluster_graph_louvain = cluster_graph_louvain_df$cell_membership,
      cluster_graph_louvain_clustree = plot_clustree(cluster_graph_louvain, cluster_graph_louvain_resolutions, "resolution", title = !!clustree_title),
      cluster_graph_louvain_clustree_file = target(
        save_clustree(cluster_graph_louvain_clustree, fs::path(!!plots_out_dir, !!clustree_out_file)),

        format = "file"
      )
    )
  } else {
    plan_cluster_graph_louvain <- drake::drake_plan(
      cluster_graph_louvain_resolutions = NULL,
      cluster_graph_louvain_df = NULL,
      cluster_graph_louvain = NULL,
      cluster_graph_louvain_clustree = NULL,
      cluster_graph_louvain_clustree_file = NULL
    )
  }

  if (cluster_graph_walktrap_enabled) {
    plan_cluster_graph_walktrap <- drake::drake_plan(
      cluster_graph_walktrap_df = run_graph_based_clustering(graph_snn, is_integration = !!is_integration, algorithm = "walktrap", resolution = NULL),
      cluster_graph_walktrap = cluster_graph_walktrap_df$cell_membership
    )
  } else {
    plan_cluster_graph_walktrap <- drake::drake_plan(
      cluster_graph_walktrap_df = NULL,
      cluster_graph_walktrap = NULL
    )
  }

  if (cluster_graph_leiden_enabled) {
    if (is_integration) {
      clustree_title <- "cluster_int_graph_leiden"
      clustree_out_file <- "cluster_int_graph_leiden_clustree.pdf"
    } else {
      clustree_title <- "cluster_graph_leiden"
      clustree_out_file <- "cluster_graph_leiden_clustree.pdf"
    }

    plan_cluster_graph_leiden <- drake::drake_plan(
      cluster_graph_leiden_resolutions = !!cluster_graph_leiden_resolutions_,
      cluster_graph_leiden_df = target(
        run_graph_based_clustering(graph_snn, is_integration = !!is_integration, algorithm = "leiden", resolution = cluster_graph_leiden_resolutions),

        dynamic = map(cluster_graph_leiden_resolutions)
      ),
      cluster_graph_leiden = cluster_graph_leiden_df$cell_membership,
      cluster_graph_leiden_clustree = plot_clustree(cluster_graph_leiden, cluster_graph_leiden_resolutions, "resolution", title = !!clustree_title),
      cluster_graph_leiden_clustree_file = target(
        save_clustree(cluster_graph_leiden_clustree, fs::path(!!plots_out_dir, !!clustree_out_file)),

        format = "file"
      )
    )
  } else {
    plan_cluster_graph_leiden <- drake::drake_plan(
      cluster_graph_leiden_resolutions = NULL,
      cluster_graph_leiden_df = NULL,
      cluster_graph_leiden = NULL,
      cluster_graph_leiden_clustree = NULL,
      cluster_graph_leiden_clustree_file = NULL
    )
  }

  drake::bind_plans(plan_cluster_graph_common, plan_cluster_graph_louvain, plan_cluster_graph_walktrap, plan_cluster_graph_leiden)
}

#' @title Get a subplan for k-means clustering.
#' @description Besides k-means for selected number of `k`s, there is also a best `k` algorithm.
#' @param sce_target_name A character scalar: name of target representing a SCE object that will be used for k-means clustering.
#' @param dimred A character scalar: which `reducedDim()` to use for clustering.
#' @param cluster_kmeans_k_enabled,cluster_kmeans_kbest_enabled
#'   A logical scalar: if `FALSE`, disable the selected clustering and set all corresponding targets to `NULL`.
#' @param cluster_kmeans_k An integer vector: `k`s for k-means.
#' @param is_integration A logical scalar: if `TRUE`, clustering results will be named as `cluster_int_kmeans_<method>[_k<k>]`.
#'   If `FALSE`, they will be named as `cluster_kmeans_<method>[_k<k>]`.
#' @param plots_out_dir A character scalar: path to output directory to save plots (e.g. [clustree::clustree()]).
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_clustering
#' @export
get_clustering_kmeans_subplan <- function(sce_target_name,
                                          dimred,
                                          cluster_kmeans_k_enabled,
                                          cluster_kmeans_k,
                                          cluster_kmeans_kbest_enabled,
                                          is_integration,
                                          plots_out_dir) {
  if (cluster_kmeans_k_enabled) {
    if (is_integration) {
      clustree_title <- "cluster_int_kmeans_k, cluster_int_kmeans_kbest"
      clustree_out_file <- "cluster_int_kmeans_k_clustree.pdf"
    } else {
      clustree_title <- "cluster_kmeans_k, cluster_kmeans_kbest"
      clustree_out_file <- "cluster_kmeans_k_clustree.pdf"
    }

    plan_cluster_kmeans_k <- drake::drake_plan(
      cluster_kmeans_k_params = !!cluster_kmeans_k,
      cluster_kmeans_k_df = target(
        run_kmeans_clustering(!!sym(sce_target_name), kmeans_k = cluster_kmeans_k_params, is_integration = !!is_integration, dimred = !!dimred),

        dynamic = map(cluster_kmeans_k_params)
      ),
      cluster_kmeans_k = cluster_kmeans_k_df$cell_membership,
      cluster_kmeans_k_clustree = plot_clustree(
        c(cluster_kmeans_k, cluster_kmeans_kbest), c(cluster_kmeans_k_params, cluster_kmeans_kbest_k), "k",
        title = !!clustree_title
      ),
      cluster_kmeans_k_clustree_file = target(
        save_clustree(cluster_kmeans_k_clustree, fs::path(!!plots_out_dir, !!clustree_out_file)),

        format = "file"
      )
    )
  } else {
    plan_cluster_kmeans_k <- drake::drake_plan(
      cluster_kmeans_k_params = NULL,
      cluster_kmeans_k_df = NULL,
      cluster_kmeans_k = NULL,
      cluster_kmeans_k_clustree = NULL,
      cluster_kmeans_k_clustree_file = NULL
    )
  }

  if (cluster_kmeans_kbest_enabled) {
    if (is_integration) {
      kbest_gaps_plot_out_file <- "cluster_int_kmeans_kbest_gaps.pdf"
      kbest_sce_column <- "cluster_int_kmeans_kbest_k"
    } else {
      kbest_gaps_plot_out_file <- "cluster_kmeans_kbest_gaps.pdf"
      kbest_sce_column <- "cluster_kmeans_kbest_k"
    }

    plan_cluster_kmeans_kbest <- drake::drake_plan(
      cluster_kmeans_kbest_gaps = cluster::clusGap(reducedDim(!!sym(sce_target_name), "pca"), kmeans, K.max = 20, nstart = 5, B = 25),
      cluster_kmeans_kbest_k = cluster::maxSE(cluster_kmeans_kbest_gaps$Tab[, "gap"], cluster_kmeans_kbest_gaps$Tab[, "SE.sim"]),
      cluster_kmeans_kbest_gaps_plot = make_kmeans_gaps_plot(cluster_kmeans_kbest_gaps, cluster_kmeans_kbest_k),
      cluster_kmeans_kbest_gaps_plot_file = target(
        ggplot2::ggsave(fs::path(!!plots_out_dir, !!kbest_gaps_plot_out_file), cluster_kmeans_kbest_gaps_plot),

        format = "file"
      ),
      cluster_kmeans_kbest_df = run_kmeans_clustering(!!sym(sce_target_name), kmeans_k = cluster_kmeans_kbest_k, is_integration = !!is_integration, dimred = !!dimred) %>%
        dplyr::mutate(algorithm = "kbest", subtitle = glue("Best K, k = {k}"), sce_column = glue("{x}{k}", x = !!kbest_sce_column)),
      cluster_kmeans_kbest = cluster_kmeans_kbest_df$cell_membership
    )
  } else {
    plan_cluster_kmeans_kbest <- drake::drake_plan(
      cluster_kmeans_kbest_gaps = NULL,
      cluster_kmeans_kbest_k = NULL,
      cluster_kmeans_kbest_gaps_plot = NULL,
      cluster_kmeans_kbest_gaps_plot_file = NULL,
      cluster_kmeans_kbest_df = NULL,
      cluster_kmeans_kbest = NULL
    )
  }

  drake::bind_plans(plan_cluster_kmeans_k, plan_cluster_kmeans_kbest)
}

#' @title Get a subplan for SC3 clustering.
#' @param sce_target_name A character scalar: name of target representing a SCE object that will be used for SC3 clustering.
#' @param cluster_sc3_enabled A logical scalar: if `FALSE`, disable the selected clustering and set all corresponding targets to `NULL`.
#' @param cluster_sc3_k_ An integer vector: `k`s for SC3.
#' @param cluster_sc3_n_cores An integer scalar: number of cores to use for parallel execution of the SC3 algorithm.
#' @param is_integration A logical scalar: if `TRUE`, clustering results will be named as `cluster_int_sc3_k<k>`.
#'   If `FALSE`, they will be named as `cluster_sc3_k<k>`.
#' @param plots_out_dir A character scalar: path to output directory to save plots (e.g. [clustree::clustree()]).
#' @param seed An integer scalar: random seed for SC3.
#' @return [drake::drake_plan()]
#'
#' @concept get_subplan_clustering
#' @export
get_clustering_sc3_subplan <- function(sce_target_name, cluster_sc3_enabled, cluster_sc3_k_, cluster_sc3_n_cores, is_integration, plots_out_dir, seed = 1) {
  if (cluster_sc3_enabled) {
    if (is_integration) {
      clustree_title <- "cluster_int_sc3"
      clustree_out_file <- "cluster_int_sc3_clustree.pdf"
    } else {
      clustree_title <- "cluster_sc3"
      clustree_out_file <- "cluster_sc3_clustree.pdf"
    }

    plan_cluster_sc3 <- drake::drake_plan(
      cluster_sc3_k = !!cluster_sc3_k_,
      cluster_sce_sc3_bpparam = cluster_sce_sc3_bpparam_fn(!!cluster_sc3_n_cores, seed = !!seed),
      cluster_sce_sc3 = calc_sc3(
        !!sym(sce_target_name),
        sc3_k = cluster_sc3_k,
        is_integration = !!is_integration,
        BPPARAM = cluster_sce_sc3_bpparam
      ),
      cluster_sc3_df = cluster_sc3_df_fn(cluster_sce_sc3, sc3_k = cluster_sc3_k, is_integration = !!is_integration),
      cluster_sc3 = cluster_sc3_df$cell_membership,
      cluster_sc3_clustree = plot_clustree(cluster_sc3, cluster_sc3_k, "k", title = !!clustree_title),
      cluster_sc3_clustree_file = target(
        save_clustree(cluster_sc3_clustree, fs::path(!!plots_out_dir, !!clustree_out_file)),

        format = "file"
      ),
      cluster_sc3_cluster_stability_plots_file = target(
        cluster_sc3_cluster_stability_plots_file_fn(cluster_sc3_df, !!plots_out_dir)$output_file,

        format = "file"
      )
    )
  } else {
    plan_cluster_sc3 <- drake::drake_plan(
      cluster_sce_sc3_bpparam = NULL,
      cluster_sce_sc3 = NULL,
      cluster_sc3_df = NULL,
      cluster_sc3 = NULL,
      cluster_sc3_cluster_stability_plots_file = NULL
    )
  }

  plan_cluster_sc3
}

#' @title Get subplan for clustering.
#' @description Besides clustering itself, this plan also includes dimred plots of computed clusters.
#' @param cfg A list of parameters for `02_norm_clustering` or `02_int_clustering` stage.
#' @param sce_clustering_target_name,sce_dimred_plots_target_name
#'   A character scalar: name of target representing a SCE object that will be used for SC3 clustering and dimred plots, respectively.
#' @param dimred A character scalar: which `reducedDim()` to use for clustering. This is only applied to graph-based and k-means clustering.
#' @param report_dimred_names A character vector: dimreds to use for plotting clustering results.
#' @param dimred_plots_out_dir,other_plots_out_dir A character scalar: path to output directory to save plots.
#' @param is_integration A logical scalar: if `TRUE`, clustering results will be named with `cluster_int_*` prefix.
#' @param seed An integer scalar: random seed for SC3.
#' @return A combined [drake::drake_plan()] from:
#'
#' - [get_clustering_graph_subplan()]
#' - [get_clustering_kmeans_subplan()]
#' - [get_clustering_sc3_subplan()]
#'
#' @concept get_subplan_clustering
#' @export
get_clustering_subplan <- function(cfg,
                                   sce_clustering_target_name,
                                   sce_dimred_plots_target_name,
                                   dimred,
                                   report_dimred_names,
                                   dimred_plots_out_dir,
                                   other_plots_out_dir,
                                   is_integration,
                                   seed = 1) {
  any_clustering_enabled <- any(
    cfg$CLUSTER_GRAPH_LOUVAIN_ENABLED, cfg$CLUSTER_GRAPH_WALKTRAP_ENABLED, cfg$CLUSTER_GRAPH_LEIDEN_ENABLED,
    cfg$CLUSTER_KMEANS_K_ENABLED, cfg$CLUSTER_KMEANS_KBEST_ENABLED,
    cfg$CLUSTER_SC3_ENABLED
  )

  plan_clustering_graph <- get_clustering_graph_subplan(
    sce_target_name = sce_clustering_target_name,
    dimred = dimred,
    cluster_graph_louvain_enabled = cfg$CLUSTER_GRAPH_LOUVAIN_ENABLED,
    cluster_graph_louvain_resolutions_ = cfg$CLUSTER_GRAPH_LOUVAIN_RESOLUTIONS,
    cluster_graph_walktrap_enabled = cfg$CLUSTER_GRAPH_WALKTRAP_ENABLED,
    cluster_graph_leiden_enabled = cfg$CLUSTER_GRAPH_LEIDEN_ENABLED,
    cluster_graph_leiden_resolutions_ = cfg$CLUSTER_GRAPH_LEIDEN_RESOLUTIONS,
    cluster_graph_snn_k_ = cfg$CLUSTER_GRAPH_SNN_K,
    cluster_graph_snn_type_ = cfg$CLUSTER_GRAPH_SNN_TYPE,
    is_integration = is_integration,
    plots_out_dir = other_plots_out_dir
  )
  plan_clustering_kmeans <- get_clustering_kmeans_subplan(
    sce_target_name = sce_clustering_target_name,
    dimred = dimred,
    cluster_kmeans_k_enabled = cfg$CLUSTER_KMEANS_K_ENABLED,
    cluster_kmeans_k = cfg$CLUSTER_KMEANS_K,
    cluster_kmeans_kbest_enabled = cfg$CLUSTER_KMEANS_KBEST_ENABLED,
    is_integration = is_integration,
    plots_out_dir = other_plots_out_dir
  )
  plan_clustering_sc3 <- get_clustering_sc3_subplan(
    sce_target_name = sce_clustering_target_name,
    cluster_sc3_enabled = cfg$CLUSTER_SC3_ENABLED,
    cluster_sc3_k_ = cfg$CLUSTER_SC3_K,
    cluster_sc3_n_cores = cfg$CLUSTER_SC3_N_CORES,
    is_integration = is_integration,
    plots_out_dir = other_plots_out_dir,
    seed = seed
  )

  if (any_clustering_enabled) {
    plan_clusters_all <- drake::drake_plan(
      clusters_all = c(
        cluster_graph_walktrap, cluster_graph_louvain, cluster_graph_leiden,
        cluster_kmeans_kbest, cluster_kmeans_k,
        cluster_sc3
      ),

      clusters_all_df = dplyr::bind_rows(
        cluster_graph_walktrap_df, cluster_graph_louvain_df, cluster_graph_leiden_df,
        cluster_kmeans_kbest_df, cluster_kmeans_k_df,
        cluster_sc3_df
      ),
      dimred_plots_clustering = target(
        dimred_plots_clustering_fn(
          !!sym(sce_dimred_plots_target_name),
          dimred_names = !!report_dimred_names,
          cluster_df = dplyr::select(clusters_all_df, -data),
          out_dir = !!dimred_plots_out_dir
        ),

        dynamic = map(clusters_all_df),
        format = "qs"
      ),
      dimred_plots_clustering_files = dplyr::select(dimred_plots_clustering, -dimred_plot),
      dimred_plots_clustering_files_out = target(
        c(dimred_plots_clustering_files$dimred_plot_out_pdf_file, dimred_plots_clustering_files$dimred_plot_out_png_file),

        format = "file"
      ),
      dimred_plots_clustering_group_by_list = c(
        dplyr::filter(dimred_plots_clustering, algorithm_category != "kmeans") %>%
          dplyr::group_split(algorithm_category, algorithm, dimred_name),
        dplyr::filter(dimred_plots_clustering, algorithm_category == "kmeans") %>%
          dplyr::mutate(algorithm = "k") %>%
          dplyr::group_split(dimred_name)
      ),
      dimred_plots_clustering_united_files = target(
        dimred_plots_clustering_united_files_fn(
          dimred_plots_clustering_group_by_list,
          out_dir = !!dimred_plots_out_dir
        ),

        dynamic = map(dimred_plots_clustering_group_by_list)
      ),
      dimred_plots_clustering_united_files_out = target(
        dimred_plots_clustering_united_files$dimred_plot_out_pdf_file,

        format = "file"
      )
    )
  } else {
    plan_clusters_all <- drake::drake_plan(
      clusters_all = NULL,
      clusters_all_df = NULL,
      dimred_plots_clustering = NULL,
      dimred_plots_clustering_files = NULL,
      dimred_plots_clustering_files_out = NULL,
      dimred_plots_clustering_group_by_list = NULL,
      dimred_plots_clustering_united_files = NULL,
      dimred_plots_clustering_united_files_out = NULL
    )
  }

  drake::bind_plans(plan_clustering_graph, plan_clustering_kmeans, plan_clustering_sc3, plan_clusters_all)
}
